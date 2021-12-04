;;; citar.el --- Citation-related commands for org, latex, markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Bruce D'Arcus

;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.9
;; Homepage: https://github.com/bdarcus/citar
;; Package-Requires: ((emacs "27.1") (s "1.12") (parsebib "3.0") (org "9.5"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  A completing-read front-end to browse, filter and act on BibTeX, BibLaTeX,
;;  and CSL JSON bibliographic data, including LaTeX, markdown, and org-cite
;;  citation editing support.
;;
;;  With embark, it also provides access to contextual actions, both in the
;;  minibuffer, and in the buffer at-point.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'seq)
(require 'browse-url)
(require 'citar-file)
(require 'parsebib)
(require 's)
(require 'crm)
(require 'skeleton)

;;; Declare variables for byte compiler

(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-pre-action-hooks)
(defvar embark-general-map)
(defvar embark-meta-map)
(defvar citar-org-open-note-function)
(defvar citar-file-extensions)
(defvar citar-file-note-extensions)
(defvar citar-file-open-prompt)
(defvar citar-file-variable)
(defvar citar-file-find-additional-files)

;;; Variables

(defgroup citar nil
  "Citations and bibliography management."
  :group 'editing)

(defface citar
  '((t :inherit font-lock-doc-face))
  "Default Face for `citar' candidates."
  :group 'citar)

(defface citar-highlight
  '((t :weight bold))
  "Face used to highlight content in `citar' candidates."
  :group 'citar)

(defcustom citar-bibliography nil
  "A list of bibliography files."
  :group 'citar
  :type '(repeat file))

(defcustom citar-library-paths nil
  "A list of files paths for related PDFs, etc."
  :group 'citar
  :type '(repeat path))

(defcustom citar-notes-paths nil
  "A list of file paths for bibliographic notes."
  :group 'citar
  :type '(repeat path))

(defcustom citar-templates
  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
    (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n"))
  "Configures formatting for the bibliographic entry.

The main and suffix templates are for candidate display, and preview
for inserting formatted references."
    :group 'citar
    :type  '(alist :key-type symbol
                   :value-type string
                   :options (main suffix preview note)))

(defcustom citar-note-skeleton
  '(nil
    (when (derived-mode-p 'org-mode) (citar-org-note-setup))
    (if (derived-mode-p 'org-mode) "#+title: " "# ")
    "Notes on "
    (if &title (format "\"%s\"" (citar-clean-string &title)) (concat "@" &=key=))
    "\n\n" _
    (when (derived-mode-p 'org-mode) "\n\n#+print_bibliography:"))
  "Skeleton for newly created notes.
See `skeleton-insert` for valid values.  Fields of the
bibliography entry are bound with names prefixed by '&'; for
example the \"=key=\" field can be accessed using &=key=, the
title using &title, etc.  The entry itself is bound to &=entry=."
  :group 'citar
  :type '(list (choice :tag "Interactor"
                       (const :tag "None" nil)
                       (string :tag "Prompt")
                       (sexp :tag "Expression"))
               (repeat :inline t :tag "Elements"
                       (choice :value "" :tag ""
                               (const :tag "New line and indent" \n)
                               (const :tag "Indent" >)
                               (string :tag "Text")
                               (const :tag "Cursor" _)
                               (sexp :tag "Eval")))))

(defcustom citar-insert-reference-function
  #'citar--insert-reference
  "A function that takes a list of (KEY . ENTRY), and returns formatted references."
  :group 'citar
  :type 'function)

(defcustom citar-display-transform-functions
  ;; TODO change this name, as it might be confusing?
  '((t  . citar-clean-string)
    (("author" "editor") . citar-shorten-names))
  "Configure transformation of field display values from raw values.

All functions that match a particular field are run in order."
  :group 'citar
  :type '(alist :key-type   (choice (const t) (repeat string))
                :value-type function))

(defcustom citar-symbols
  `((file  .  ("F" . " "))
    (note .   ("N" . " "))
    (link .   ("L" . " ")))
  "Configuration alist specifying which symbol or icon to pick for a bib entry.
This leaves room for configurations where the absense of an item
may be indicated with the same icon but a different face.

To avoid alignment issues make sure that both the car and cdr of a symbol have
the same width."
  :group 'citar
  :type '(alist :key-type symbol
                :value-type (cons (string :tag "Present")
                                  (string :tag "Absent"))
                :options (file note link)))

(defcustom citar-symbol-separator " "
  "The padding between prefix symbols."
  :group 'citar
  :type 'string)

(defcustom citar-force-refresh-hook nil
  "Hook run when user forces a (re-) building of the candidates cache.
This hook is only called when the user explicitly requests the
cache to be rebuilt.  It is intended for 'heavy' operations which
recreate entire bibliography files using an external reference
manager like Zotero or JabRef."
  :group 'citar
  :type 'hook)

(defcustom citar-default-action #'citar-open
  "The default action for the `citar-at-point' command."
  :group 'citar
  :type 'function)

(defcustom citar-at-point-fallback 'prompt
  "Fallback action for `citar-at-point'.
The action is used when no citation key is found at point.
`prompt' means choosing entries via `citar-select-keys'
and nil means no action."
  :group 'citar
  :type '(radio (const :tag "Prompt" prompt)
                (const :tag "Ignore" nil)))

(defcustom citar-open-note-function #'citar-open-note-default
  "Function to open and existing or create a new note.

A note function must take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)

If you use 'org-roam' and 'org-roam-bibtex', you can use
'orb-bibtex-actions-edit-note' for this value."
  :group 'citar
  :type '(choice (function-item citar-open-note-default)
                 (function :tag "Open note function")))

(defcustom citar-display-note-function #'pop-to-buffer
  "Function to display a note buffer.
The function should take one argument, the buffer to display."
  :group 'citar
  :type '(choice (function-item pop-to-buffer)
                 (function-item pop-to-buffer-same-window)
                 (function :tag "Buffer display function")))

(defcustom citar-note-setup-functions
  '(citar-skeleton-insert
    (lambda (&rest _) (when (boundp 'evil-state) (evil-insert 1))))
  "Functions called when new note is created.

The functions in this list are called in the buffer visiting a
newly created note.  Each function should take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)"
  :group 'citar
  :type 'hook
  :options '(citar-skeleton-insert))

(defcustom citar-at-point-function #'citar-dwim
  "The function to run for 'citar-at-point'."
  :group 'citar
  :type 'function)

(defcustom citar-major-mode-functions
  '(((org-mode) .
     ((local-bib-files . citar-org-local-bib-files)
      (insert-citation . citar-org-insert-citation)
      (insert-edit . citar-org-insert-edit)
      (key-at-point . citar-org-key-at-point)
      (citation-at-point . citar-org-citation-at-point)))
    ((latex-mode) .
     ((local-bib-files . citar-latex-local-bib-files)
      (insert-citation . citar-latex-insert-citation)
      (insert-edit . citar-latex-insert-edit)
      (key-at-point . citar-latex-key-at-point)
      (citation-at-point . citar-latex-citation-at-point)))
    ((markdown-mode) .
     ((insert-keys . citar-markdown-insert-keys)
      (insert-citation . citar-markdown-insert-citation)
      (insert-edit . citar-markdown-insert-edit)
      (key-at-point . citar-markdown-key-at-point)
      (citation-at-point . citar-markdown-citation-at-point)))
    (t .
       ((insert-keys . citar--insert-keys-comma-separated))))
  "The variable determining the major mode specific functionality.

It is alist with keys being a list of major modes.

The value is an alist with values being functions to be used for
these modes while the keys are symbols used to lookup them up.
The keys are:

local-bib-files: the corresponding functions should return the list of
local bibliography files.

insert-keys: the corresponding function should insert the list of keys given
to as the argument at point in the buffer.

insert-citation: the corresponding function should insert a
complete citation from a list of keys at point.  If the point is
in a citation, new keys should be added to the citation.

insert-edit: the corresponding function should accept an optional
prefix argument and interactively edit the citation or key at
point.

key-at-point: the corresponding function should return the
citation key at point or nil if there is none.  The return value
should be (KEY . BOUNDS), where KEY is a string and BOUNDS is a
pair of buffer positions indicating the start and end of the key.

citation-at-point: the corresponding function should return the
keys of the citation at point, or nil if there is none.  The
return value should be (KEYS . BOUNDS), where KEYS is a list of
strings and BOUNDS is pair of buffer positions indicating the
start and end of the citation."
  :group 'citar
  :type 'alist)

;;; History, including future history list.

(defvar citar-history nil
  "Search history for `citar'.")

(defcustom citar-presets nil
  "List of predefined searches."
  :group 'citar
  :type '(repeat string))

;;; Keymaps

(defvar citar-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (cons "insert citation" #'citar-insert-citation))
    (define-key map (kbd "k") (cons "insert keys" #'citar-insert-keys))
    (define-key map (kbd "fr") (cons "insert formatted reference" #'citar-insert-reference))
    (define-key map (kbd "b") (cons "insert bibtex" #'citar-insert-bibtex))
    (define-key map (kbd "o") (cons "open source document" #'citar-open))
    (define-key map (kbd "e") (cons "open bibtex entry" #'citar-open-entry))
    (define-key map (kbd "l") (cons "open source URL or DOI" #'citar-open-link))
    (define-key map (kbd "n") (cons "open notes" #'citar-open-notes))
    (define-key map (kbd "f") (cons "open library files" #'citar-open-library-files))
    (define-key map (kbd "r") (cons "refresh" #'citar-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") (cons "default action" #'citar-run-default-action))
    map)
  "Keymap for Embark minibuffer actions.")

(defvar citar-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") (cons "insert or edit" #'citar-insert-edit))
    (define-key map (kbd "c") (cons "insert citation" #'citar-insert-citation))
    (define-key map (kbd "o") (cons "open source document" #'citar-open))
    (define-key map (kbd "e") (cons "open bibtex entry" #'citar-open-entry))
    (define-key map (kbd "l") (cons "open source URL or DOI" #'citar-open-link))
    (define-key map (kbd "n") (cons "open notes" #'citar-open-notes))
    (define-key map (kbd "f") (cons "open library files" #'citar-open-library-files))
    (define-key map (kbd "r") (cons "refresh library" #'citar-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") (cons "default action" #'citar-run-default-action))
    map)
  "Keymap for Embark citation-key actions.")

;;; Completion functions

(cl-defun citar-select-refs (&optional &key rebuild-cache)
  "Select bibliographic references.

A wrapper around 'completing-read-multiple' that returns an alist
of (KEY . ENTRY), where the entry is a field-value alist.

Therefore, for each returned candidate, 'car' is the citekey, and
'cdr' is an alist of structured data.

Includes the following optional argument:

'REBUILD-CACHE' if t, forces rebuilding the cache before
offering the selection candidates."
  (if-let* ((crm-separator "\\s-*&\\s-*")
            (candidates (citar--get-candidates rebuild-cache))
            (chosen
             (completing-read-multiple
              "References: "
              (lambda (string predicate action)
                (if (eq action 'metadata)
                    `(metadata
                      (affixation-function . ,#'citar--affixation)
                      (category . citar-reference))
                  (complete-with-action action candidates string predicate)))
              nil nil nil
              'citar-history citar-presets nil)))
      (seq-map
       (lambda (choice)
         ;; Collect citation key-entry of selected candidate(s).
         (or (cdr (assoc choice candidates))
             ;; When calling embark at-point, use keys to look up and return the
             ;; selected candidates.
             ;; See https://github.com/bdarcus/citar/issues/233#issuecomment-901536901
             (cdr (seq-find (lambda (cand) (equal choice (cadr cand))) candidates))))
       chosen)
    (message "Key not found")))

(cl-defun citar-select-ref (&optional &key rebuild-cache)
  "Select a bibliographic reference.

A wrapper around 'completing-read' that returns a (KEY . ENTRY)
cons.

Includes the following optional argument:

'REBUILD-CACHE' if t, forces rebuilding the cache before
offering the selection candidates."
  (if-let* ((candidates (citar--get-candidates rebuild-cache))
            (choice
             (completing-read
              "References: "
              (lambda (string predicate action)
                (if (eq action 'metadata)
                    `(metadata
                      (affixation-function . ,#'citar--affixation)
                      (category . citar-reference))
                  (complete-with-action action candidates string predicate)))
              nil nil nil
              'citar-history citar-presets nil)))
      ;; Return result.
      (or (cdr (assoc choice candidates))
          ;; When calling embark at-point, use key to look up and return the
          ;; selected candidates.
          ;; See https://github.com/bdarcus/citar/issues/233#issuecomment-901536901
          ;; TODO
          (cdr (seq-find (lambda (cand) (equal choice (cadr cand))) candidates)))
    (message "Key not found")))

(defun citar-select-files (files)
  "Select file(s) from a list of FILES."
  ;; TODO add links to candidates
  (completing-read-multiple
   "Open related file(s): "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         `(metadata
        ; (group-function . citar-select-group-related-sources)
           (category . file))
       (complete-with-action action files string predicate)))))

(defun citar-select-group-related-sources (file transform)
  "Group by FILE by source, TRANSFORM."
    (let ((extension (file-name-extension file)))
      (when transform file
        ;; Transform for grouping and group title display.
        (cond
         ((string= extension (or "org" "md")) "Notes")
          (t "Library Files")))))

(defun citar--get-major-mode-function (key &optional default)
  "Return KEY from 'major-mode-functions'."
  (alist-get
   key
   (cdr (seq-find
         (lambda (modefns)
           (let ((modes (car modefns)))
             (or (eq t modes)
                 (apply #'derived-mode-p (if (listp modes) modes (list modes))))))
         citar-major-mode-functions))
   default))

(defun citar--major-mode-function (key default &rest args)
  "Function for the major mode corresponding to KEY applied to ARGS.
If no function is found, the DEFAULT function is called."
  (apply (citar--get-major-mode-function key default) args))

(defun citar--local-files-to-cache ()
  "The local bibliographic files not included in the global bibliography."
  ;; We cache these locally to the buffer.
  (seq-difference (citar-file--normalize-paths
                   (citar--major-mode-function 'local-bib-files #'ignore))
                  (citar-file--normalize-paths
                   citar-bibliography)))

(defun citar-get-value (field item)
  "Return the FIELD value for ITEM."
  (cdr (assoc-string field item 'case-fold)))

(defun citar-has-a-value (fields item)
  "Return the first field that has a value in ITEM among FIELDS ."
  (seq-find (lambda (field) (citar-get-value field item)) fields))

(defun citar-display-value (fields item)
  "Return the first non nil value for ITEM among FIELDS .

The value is transformed using `citar-display-transform-functions'"
  (let ((field (citar-has-a-value fields item)))
    (seq-reduce (lambda (string fun)
                  (if (or (eq t (car fun))
                          (member field (car fun)))
                      (funcall (cdr fun) string)
                    string))
                citar-display-transform-functions
            ;; Make sure we always return a string, even if empty.
                (or (citar-get-value field item) ""))))

;; Lifted from bibtex-completion
(defun citar-clean-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s)))

(defun citar-shorten-names (names)
  "Return a list of family names from a list of full NAMES.

To better accomomodate corporate names, this will only shorten
personal names of the form 'family, given'."
  (when (stringp names)
    (mapconcat
     (lambda (name)
       (if (eq 1 (length name))
           (cdr (split-string name " "))
         (car (split-string name ", "))))
     (split-string names " and ") ", ")))

(defun citar--fields-for-format (template)
  "Return list of fields for TEMPLATE."
  ;; REVIEW I don't really like this code, but it works correctly.
  ;;        Would be good to at least refactor to remove s dependency.
  (let* ((fields-rx "${\\([^}]+\\)}")
         (raw-fields (seq-mapcat #'cdr (s-match-strings-all fields-rx template))))
    (seq-map
     (lambda (field)
       (car (split-string field ":")))
     (seq-mapcat (lambda (raw-field) (split-string raw-field " ")) raw-fields))))

(defun citar--fields-in-formats ()
  "Find the fields to mentioned in the templates."
  (seq-mapcat #'citar--fields-for-format
              (list (citar-get-template 'main)
                    (citar-get-template 'suffix)
                    (citar-get-template 'note))))

(defun citar--fields-to-parse ()
  "Determine the fields to parse from the template."
  (seq-concatenate
   'list
   (citar--fields-in-formats)
   (list "doi" "url" citar-file-variable)))

(defun citar--format-candidates (bib-files &optional context)
  "Format candidates from BIB-FILES, with optional hidden CONTEXT metadata.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (let* ((candidates ())
         (raw-candidates
          (parsebib-parse bib-files :fields (citar--fields-to-parse)))
         (main-width (citar--format-width (citar-get-template 'main)))
         (suffix-width (citar--format-width (citar-get-template 'suffix)))
         (citar-file-find-additional-files nil)
         (symbols-width (string-width (citar--symbols-string t t t)))
         (star-width (- (frame-width) (+ 2 symbols-width main-width suffix-width))))
    (maphash
     (lambda (citekey entry)
       (let* (              (files
               (when (citar-file--files-for-entry
                      citekey
                      entry
                      citar-library-paths
                      citar-file-extensions)
                 " has:files"))
              (notes
               (when (citar-file--files-for-entry
                      citekey
                      nil ; don't want to check file field
                      citar-notes-paths
                      citar-file-note-extensions)
                 " has:notes"))
              (link
               (when (citar-has-a-value '("doi" "url") entry)
                 "has:link"))
              (candidate-main
               (citar--format-entry
                entry
                star-width
                (citar-get-template 'main)))
              (candidate-suffix
               (citar--format-entry
                entry
                star-width
                (citar-get-template 'suffix)))
              ;; We display this content already using symbols; here we add back
              ;; text to allow it to be searched, and citekey to ensure uniqueness
              ;; of the candidate.
              (candidate-hidden (string-join (list files notes link context citekey) " ")))
         (push
          (cons
           ;; If we don't trim the trailing whitespace,
           ;; 'completing-read-multiple' will get confused when there are
           ;; multiple selected candidates.
           (string-trim-right
            (concat
             ;; We need all of these searchable:
             ;;   1. the 'candidate-main' variable to be displayed
             ;;   2. the 'candidate-suffix' variable to be displayed with a different face
             ;;   3. the 'candidate-hidden' variable to be hidden
             (propertize candidate-main 'face 'citar-highlight) " "
             (propertize candidate-suffix 'face 'citar) " "
             (propertize candidate-hidden 'invisible t)))
           (cons citekey entry))
          candidates)))
       raw-candidates)
    candidates))

  (defun citar--affixation (cands)
    "Add affixation prefix to CANDS."
    (seq-map
     (lambda (candidate)
       (let ((candidate-symbols (citar--symbols-string
                                 (string-match "has:files" candidate)
                                 (string-match "has:note" candidate)
                                 (string-match "has:link" candidate))))
         (list candidate candidate-symbols "")))
     cands))

(defun citar--symbols-string (has-files has-note has-link)
  "String for display from booleans HAS-FILES HAS-LINK HAS-NOTE."
  (cl-flet ((thing-string (has-thing thing-symbol)
                          (if has-thing
                              (cadr (assoc thing-symbol citar-symbols))
                            (cddr (assoc thing-symbol citar-symbols)))))
    (seq-reduce (lambda (constructed newpart)
                  (let* ((str (concat constructed newpart
                                      citar-symbol-separator))
                         (pos (length str)))
                    (put-text-property (- pos 1) pos 'display
                                       (cons 'space (list :align-to (string-width str)))
                                       str)
                    str))
                (list (thing-string has-files 'file)
                      (thing-string has-note 'note)
                      (thing-string has-link 'link)
                      "")
                "")))

(defvar citar--candidates-cache 'uninitialized
  "Store the global candidates list.

Default value of 'uninitialized is used to indicate that cache
has not yet been created")

(defvar-local citar--local-candidates-cache 'uninitialized
  ;; We use defvar-local so can maintain per-buffer candidate caches.
  "Store the local (per-buffer) candidates list.")

;;;###autoload
(defun citar-refresh (&optional force-rebuild-cache scope)
  "Reload the candidates cache.

If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `citar-before-refresh-hook' hook.

If SCOPE is `global' only global cache is refreshed, if it is
`local' only local cache is refreshed.  With any other value both
are refreshed."
  (interactive (list current-prefix-arg nil))
  (when force-rebuild-cache
    (run-hooks 'citar-force-refresh-hook))
  (unless (eq 'local scope)
    (setq citar--candidates-cache
      (citar--format-candidates
        (citar-file--normalize-paths citar-bibliography))))
  (unless (eq 'global scope)
    (setq citar--local-candidates-cache
          (citar--format-candidates
           (citar--local-files-to-cache) "is:local"))))

(defun citar-get-template (template-name)
  "Return template string for TEMPLATE-NAME."
  (cdr (assoc template-name citar-templates)))

(defun citar--get-candidates (&optional force-rebuild-cache)
  "Get the cached candidates.

If the cache is unintialized, this will load the cache.

If FORCE-REBUILD-CACHE is t, force reload the cache."
  (unless (or citar-bibliography (citar--local-files-to-cache))
    (error "Make sure to set citar-bibliography and related paths"))
  (when force-rebuild-cache
    (citar-refresh force-rebuild-cache))
  (when (eq 'uninitialized citar--candidates-cache)
    (citar-refresh nil 'global))
  (when (eq 'uninitialized citar--local-candidates-cache)
    (citar-refresh nil 'local))
  (seq-concatenate 'list
                   citar--local-candidates-cache
                   citar--candidates-cache))

(defun citar--get-entry (key)
  "Return the cached entry for KEY."
    (cddr (seq-find
           (lambda (entry)
             (string-equal key (cadr entry)))
           (citar--get-candidates))))

(defun citar-get-link (entry)
  "Return a link for an ENTRY."
  (let* ((field (citar-has-a-value '(doi pmid pmcid url) entry))
         (base-url (pcase field
                     ('doi "https://doi.org/")
                     ('pmid "https://www.ncbi.nlm.nih.gov/pubmed/")
                     ('pmcid "https://www.ncbi.nlm.nih.gov/pmc/articles/"))))
    (when field
      (concat base-url (citar-get-value field entry)))))

(defun citar--extract-keys (keys-entries)
  "Extract list of keys from KEYS-ENTRIES alist."
  (seq-map #'car keys-entries))

;;;###autoload
(defun citar-insert-preset ()
  "Prompt for and insert a predefined search."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (when-let ((enable-recursive-minibuffers t)
             (search (completing-read "Preset: " citar-presets)))
    (insert search)))

;;; Formatting functions

(defun citar--format-width (format-string)
  "Calculate minimal width needed by the FORMAT-STRING."
  (let ((content-width (apply #'+
                              (seq-map #'string-to-number
                                       (split-string format-string ":"))))
        (whitespace-width (string-width (s-format format-string
                                                  (lambda (_) "")))))
    (+ content-width whitespace-width)))

(defun citar--fit-to-width (value width)
  "Propertize the string VALUE so that only the WIDTH columns are visible."
  (let* ((truncated-value (truncate-string-to-width value width))
         (display-value (truncate-string-to-width truncated-value width 0 ?\s)))
    (if (> (string-width value) width)
        (concat display-value (propertize (substring value (length truncated-value))
                                          'invisible t))
      display-value)))

(defun citar--format-entry (entry width format-string)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width for the * field, and the display format is governed by
FORMAT-STRING."
  ;; TODO remove s-format dependency, generalize to allow 'truncate' option
  (s-format
   format-string
   (lambda (raw-field)
     (let* ((field (split-string raw-field ":"))
            (field-names (split-string (car field) "[ ]+"))
            (field-width (string-to-number (cadr field)))
            (display-width (if (> field-width 0)
                               ;; If user specifies field width of "*", use
                               ;; WIDTH; else use the explicit 'field-width'.
                               field-width
                             width))
            ;; Make sure we always return a string, even if empty.
            (display-value (citar-display-value field-names entry)))
       (citar--fit-to-width display-value display-width)))))

(defun citar--format-entry-no-widths (entry format-string)
  "Format ENTRY for display per FORMAT-STRING."
  (s-format
   format-string
   (lambda (raw-field)
     (let ((field-names (split-string raw-field "[ ]+")))
       (citar-display-value field-names entry)))))

;;; At-point functions for Embark

;;;###autoload
(defun citar-key-finder ()
  "Return the citation key at point."
  (when-let (key (and (not (minibufferp))
                      (citar--major-mode-function 'key-at-point #'ignore)))
    (cons 'citar-key key)))

;;;###autoload
(defun citar-citation-finder ()
  "Return the keys of the citation at point."
  (when-let (citation (and (not (minibufferp))
                           (citar--major-mode-function 'citation-at-point #'ignore)))
    `(citar-citation ,(citar--stringify-keys (car citation)) . ,(cdr citation))))

(defun citar--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

;;;###autoload
(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'citar-citation-finder)
  (add-to-list 'embark-target-finders 'citar-key-finder))


(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(citar-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citar-key . citar-citation-map))
  (add-to-list 'embark-keymap-alist '(citar-citation . citar-citation-map))
  (add-to-list 'embark-pre-action-hooks '(citar-insert-edit embark--ignore-target)))

;;; Commands

;;;###autoload
(defun citar-open (keys-entries)
  "Open related resources (links or files) for KEYS-ENTRIES."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (when (and citar-library-paths
             (stringp citar-library-paths))
    (message "Make sure 'citar-library-paths' is a list of paths"))
  (let* ((files
         (citar-file--files-for-multiple-entries
          keys-entries
          (append citar-library-paths citar-notes-paths)
          (append citar-file-extensions citar-file-note-extensions)))
         (links
          (seq-map
           (lambda (key-entry)
             (citar-get-link (cdr key-entry)))
           keys-entries))
         (resource-candidates (delete-dups (append files (remq nil links))))
         (resources
          (when resource-candidates
            (completing-read-multiple "Related resources: " resource-candidates))))
    (if resource-candidates
        (dolist (resource resources)
          (cond ((string-match "http" resource 0)
                 (browse-url resource))
                (t (citar-file-open resource))))
      (message "No associated resources"))))

(defun citar--library-files-action (keys-entries action)
  "Run ACTION on files associated with KEYS-ENTRIES."
  (if-let ((fn (pcase action
                 ('open 'citar-file-open)
                 ('attach 'mml-attach-file)))
           (files
            (citar-file--files-for-multiple-entries
             keys-entries
             citar-library-paths
             citar-file-extensions)))
      (if (and citar-file-open-prompt
               (> (length files) 1))
          (let ((selected-files
                 (citar-select-files files)))
            (dolist (file selected-files)
              (funcall fn file)))
        (dolist (file files)
          (funcall fn file)))
    (message "No associated file")))

;;;###autoload
(defun citar-open-library-files (keys-entries)
 "Open library files associated with the KEYS-ENTRIES.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
    (when (and citar-library-paths
               (stringp citar-library-paths))
      (message "Make sure 'citar-library-paths' is a list of paths"))
    (citar--library-files-action keys-entries 'open))

(make-obsolete 'citar-open-pdf
               'citar-open-library-files "1.0")

;;;###autoload
(defun citar-open-notes (keys-entries)
  "Open notes associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (when (and (null citar-notes-paths)
             (equal citar-open-note-function
                    'citar-org-open-notes-default))
    (message "You must set 'citar-notes-paths' to open notes with default notes function"))
  (dolist (key-entry keys-entries)
    (funcall citar-open-note-function
             (car key-entry) (cdr key-entry))))

;;;###autoload
(defun citar-open-entry (key-entry)
  "Open bibliographic entry associated with the KEY-ENTRY.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (let ((key (citar--extract-keys key-entry)))
    (citar--open-entry (car key))))

(defun citar--open-entry (key)
  "Open bibliographic entry asociated with the KEY."
  (let ((bibtex-files
         (seq-concatenate 'list citar-bibliography (citar--local-files-to-cache))))
    (bibtex-find-entry key t nil t)))

;;;###autoload
(defun citar-insert-bibtex (keys-entries)
  "Insert bibliographic entry associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (let ((keys (citar--extract-keys keys-entries)))
    (dolist (key keys)
      (citar--insert-bibtex key))))

(defun citar--insert-bibtex (key)
  "Insert the bibtex entry for KEY at point."
  (let ((bibtex-files
         (seq-concatenate 'list citar-bibliography (citar--local-files-to-cache))))
    (insert
     (with-temp-buffer
       (dolist (bib-file bibtex-files)
         (insert-file-contents bib-file))
       (bibtex-find-entry key)
       (let ((beg (bibtex-beginning-of-entry))
             (end (bibtex-end-of-entry)))
         (buffer-substring-no-properties beg end))) "\n")))

;;;###autoload
(defun citar-open-link (keys-entries)
  "Open URL or DOI link associated with the KEYS-ENTRIES in a browser.

With prefix, rebuild the cache before offering candidates."
  ;;      (browse-url-default-browser "https://google.com")
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (dolist (key-entry keys-entries)
    (let ((link (citar-get-link (cdr key-entry))))
      (if link
          (browse-url-default-browser link)
        (message "No link found for %s" key-entry)))))

;;;###autoload
(defun citar-insert-citation (keys-entries)
  "Insert citation for the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (citar--major-mode-function
   'insert-citation
   (lambda (&rest _)
     (message "Citation insertion is not supported for %s" major-mode))
   (citar--extract-keys keys-entries)))

(defun citar-insert-edit (&optional arg)
  "Edit the citation at point."
  (interactive "P")
  (citar--major-mode-function
   'insert-edit
   (lambda (&rest _)
     (message "Citation editing is not supported for %s" major-mode))
   arg))

;;;###autoload
(defun citar-insert-reference (keys-entries)
  "Insert formatted reference(s) associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (apply citar-insert-reference-function (list keys-entries)))

(defun citar--insert-reference (keys-entries)
  "Insert formatted reference(s) associated with the KEYS-ENTRIES."
  (let ((template (citar-get-template 'preview)))
    (dolist (key-entry keys-entries)
      (when template
        (insert (citar--format-entry-no-widths (cdr key-entry) template))))))

;;;###autoload
(defun citar-insert-keys (keys-entries)
  "Insert KEYS-ENTRIES citekeys.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (citar--major-mode-function
   'insert-keys
   #'citar--insert-keys-comma-separated
   (citar--extract-keys keys-entries)))

(defun citar--insert-keys-comma-separated (keys)
  "Insert comma separated KEYS."
  (insert (string-join keys ", ")))

;;;###autoload
(defun citar-attach-library-files (keys-entries)
  "Attach library files associated with KEYS-ENTRIES to outgoing MIME message.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (when (and citar-library-paths
             (stringp citar-library-paths))
    (message "Make sure 'citar-library-paths' is a list of paths"))
  (citar--library-files-action keys-entries 'attach))

(make-obsolete 'citar-add-pdf-attachment 'citar-attach-library-files "0.9")

;;;###autoload
(defun citar-run-default-action (keys)
  "Run the default action `citar-default-action' on KEYS."
  (let* ((keys-parsed
          (if (stringp keys)
              (split-string keys " & ")
            (split-string (cdr keys) " & ")))
         (keys-entries
          (seq-map
           (lambda (key)
             (cons key (citar--get-entry key))) keys-parsed)))
    (funcall citar-default-action keys-entries)))

;;;###autoload
(defun citar-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((keys (or (car (citar--major-mode-function 'citation-at-point #'ignore))
                     (list (car (citar--major-mode-function 'key-at-point #'ignore))))))
      ;; FIX how?
      (citar-run-default-action (citar--stringify-keys keys))))

;;; Notes

(defun citar-open-note-default (key entry)
  "Open a note file from KEY and ENTRY."
  (when-let* ((file (car (citar-file--get-note-filenames
                          key
                          citar-notes-paths citar-file-note-extensions)))
              (buffer (find-file-noselect (car file))))
    (when (bufferp buffer)
      (funcall citar-display-note-function buffer)
      (when (eq 'new (cdr file))
        (with-current-buffer buffer
            (run-hook-with-args 'citar-note-setup-functions key entry))))))

(defun citar-skeleton-insert (key entry &optional skeleton regions str)
  "Wrapper around `skeleton-insert` that binds KEY and the fields of ENTRY.
When SKELETON is nil, use the value of `citar-note-skeleton`; see
its documentation for details on which variables are bound when
evaluating SKELETON.  See `skeleton-insert` for details on
SKELETON, REGIONS, and STR."
  (let* ((required '("title" "author" "editor"))      ; bound to nil if missing
         (extra `((&=key= ',key) (&=entry= ',entry))) ; if not added by parser
         (fields (seq-map
                  (lambda (field)
                    `(,(intern (concat "&" field)) ',(cdr (assoc field entry))))
                  (seq-uniq (append required (seq-map #'car entry)))))
         (skeleton-further-elements (append skeleton-further-elements fields extra)))
    (skeleton-insert (or skeleton citar-note-skeleton) regions str)))

(provide 'citar)
;;; citar.el ends here
