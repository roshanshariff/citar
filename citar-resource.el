;;; citar-resource.el --- Open resources in citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus and Roshan Shariff
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;;  Functions for opening files, notes, and URLs related to a source.
;;
;;; Code:

(require 'citar)
(require 'citar-file)

(defgroup citar-resource nil
  "Opening resources associated with bibliography items."
  :group 'citar)

(defcustom citar-resource-file-paths nil
  "List of directories to search for files."
  :group 'citar-resource
  :type '(repeat directory))

(defcustom citar-resource-file-extensions '("pdf" "djvu" "html")
  "List of extensions to search for associated files."
  :group 'citar-resource
  :type '(repeat string))

(defcustom citar-resource-file-fields '("file")
  "Bibliography entry fields in which to look for files."
  :group 'citar-resource
  :type '(repeat string :tag "Field name"))

(defcustom citar-resource-file-parser-functions '(citar-file-parser-default)
  "Functions to parse file fields."
  :group 'citar-resource
  :type '(repeat function :tag "Parser functions"))

(defcustom citar-resource-note-paths nil
  "List of directories to search for notes."
  :group 'citar-resource
  :type '(repeat directory))

(defcustom citar-resource-note-extensions '("org")
  "List of valid extensions for associated notes."
  :group 'citar-resource
  :type '(repeat string))

(defcustom citar-resource-finders '(citar-resource--file-field-finder
                                    citar-resource--file-finder
                                    citar-resource--note-finder
                                    citar-resource--url-id-finder
                                    citar-resource--url-finder)
  "Functions that return resources associated with bibliography items."
  :group 'citar-resource
  :type '(repeat function))

(defcustom citar-resource-equiv-predicates '((file citar-resource--file-equiv-p)
                                             (note citar-resource--file-equiv-p)
                                             (url citar-resource--url-equiv-p))
  "Predicates to test if resources are equivalent."
  :group 'citar-resource
  :type '(alist :key-type symbol
                :value-type (repeat function)
                :options (file note url)))

(defcustom citar-resource-open-functions '((file citar-resource--open-file)
                                           (note citar-resource--open-note)
                                           (url citar-resource--open-url))
  "Functions to open resources."
  :group 'citar-resource
  :type '(alist :key-type symbol
                :value-type (repeat function)
                :options (file note url)))

(defcustom citar-resource-groups '((file . "File")
                                   (note . "Note")
                                   (url . "URL"))
  "Names for resource types for grouping in completing-read."
  :type '(alist :key-type symbol
                :value-type string
                :options (file note url)))

(defcustom citar-resource-file-open-function #'find-file
  "Function to open a file."
  :group 'citar-resource
  :type 'function)

(defcustom citar-resource-note-open-function #'find-file
  "Function to open a note."
  :group 'citar-resource
  :type 'function)

(defcustom citar-resource-url-open-function #'browse-url
  "Function to open a URL."
  :group 'citar-resource
  :type 'function)

(defcustom citar-resource-url-identifier-alist
  '((doi "DOI" "https://doi.org/%s"
         "^https?://\\(?:dx\\.\\)?doi\\.org/\\(.*\\)$")
    (arxiv "arXiv" "https://arxiv.org/abs/%s"
           "^https?://\\(?:www\\.\\)?arxiv\\.org/\\(?:abs\\|pdf\\)/\\(.*?\\)\\(?:.pdf\\)?$")
    (pmid "PubMed" "https://www.ncbi.nlm.nih.gov/pubmed/%s"
          "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/pubmed/\\(.*\\)$")
    (pmcid "PubMed Central" "https://www.ncbi.nlm.nih.gov/pmc/articles/%s"
           "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/pmc/articles/\\(.*\\)$")
    (jstor "JSTOR" "https://www.jstor.org/stable/%s"
           "^https?://www\\.jstor\\.org/stable"))
  "Online resources that can be converted to and from URLs."
  :group 'citar-resource
  :type '(alist :key-type symbol
                :value-type (list (string :tag "Name")
                                  (string :tag "URL Format")
                                  (regexp :tag "URL Regular Expression"))))

(defcustom citar-resource-recognize-urls t
  "Recognize URLs corresponding to online resources.
When set to t, recognize URL patterns listed in
`citar-resource-url-identifier-alist'.  To recognize only some
URL patterns, use a list containing keys of
`citar-resource-url-identifier-alist'.  Set to nil to disable URL
recognition."
  :group 'citar-resource
  :type '(choice (const :tag "All Websites" t)
                 (const :tag "Never" nil)
                 (repeat :tag "Identifiers" symbol)))

(defcustom citar-resource-normalize-urls nil
  "Change recognized URLs to the standard form for their online source.
When opening URLs that match an entry of
`citar-resource-url-identifier-alist`, use the standard form of
the URL.  When set to t, standardize all recognized URLs.  To
standardize only the URLs of some sources, set to a list
containing keys of `citar-resource-url-identifier-alist`.  Set to
nil to always open the original URL."
  :group 'citar-resource
  :type '(choice (const :tag "All Websites" t)
                 (const :tag "Never" nil)
                 (repeat :tag "Identifiers" symbol)))

;;;; URLs

(defun citar-resource--get-url (urlspec)
  "Return URL corresponding to URLSPEC.
URLSPEC is either a string URL or a pair (TYPE . ID), where TYPE
is a key of `citar-resource-url-identifier-alist` and ID is a
string identifying the online resource."
  ;; (if (stringp urlspec)
  ;;     urlspec
  ;;   (when-let* ((type (car-safe urlspec))
  ;;               (id (cdr-safe urlspec))
  ;;               ()))
  (pcase urlspec
    ((and (pred stringp) url) url)      ; URLSPEC is a string
    ((and `(,type . ,(and (pred stringp) id)) ; cons of TYPE and string ID
          (let `(,_name ,urlfmt . ,_)
            (alist-get type citar-resource-url-identifier-alist)))
     (format urlfmt id))))

(defun citar-resource--recognize-url (url)
  "Recognize URL matching a known online source.
Returns (DISPLAY . URLSPEC), where DISPLAY is a human-readable
representation of the URL and URLSPEC is either (TYPE . ID) if it
was recognized or the original URL string otherwise.  See
`citar-resource--get-url` for a description of URLSPEC.

Recognizes only URL types allowed by
`citar-resource-recognize-urls`.  When
`citar-resource-normalize-urls` is not t and does not contain the
recognized TYPE, then URLSPEC is the original URL instead
of (TYPE . ID)."
  (catch 'recognized-url
    (when citar-resource-recognize-urls
      (save-match-data
        (pcase-dolist (`(,type ,name ,urlfmt ,regexp)
                       citar-resource-url-identifier-alist)
          (when-let ((id (and regexp
                              (or (eq t citar-resource-recognize-urls)
                                  (memq type citar-resource-recognize-urls))
                              (string-match regexp url)
                              (match-string 1 url))))
            (throw 'recognized-url
                   (cons (format "%s: %s" name id)
                         (if (or (eq t citar-resource-normalize-urls)
                                 (memq type citar-resource-normalize-urls))
                             (cons type id)
                           url)))))))
    ;; Unrecognized URL
    (cons url url)))

(defun citar-resource--url-finder (_key entry)
  "Find a URL given in the \"url\" field of ENTRY."
  (when-let ((url (citar-get-value 'url entry)))
    (list (cons 'url (citar-resource--recognize-url url)))))

(defun citar-resource--url-id-finder (_key entry)
  "Find URLs specified by \"doi\", \"pmid\", and \"pmcid\" fields of ENTRY."
  (let ((urls nil))
    (dolist (type '(doi pmid pmcid) (nreverse urls))
      (when-let ((id (citar-get-value type entry)))
        (push (cons 'url (cons type id)) urls)))))

(defun citar-resource--url-equiv-p (first second)
  "Test whether FIRST and SECOND are equivalent URLs."
  (cl-flet ((normalize (urlspec)
                       (cond
                        ((eq t citar-resource-normalize-urls) ; already normalized
                         urlspec)
                        ((stringp urlspec) ; try to normalize
                         (let ((citar-resource-normalize-urls t))
                           (cdr (citar-resource--recognize-url urlspec))))
                        (t urlspec))))
    (equal (normalize first) (normalize second))))

(defun citar-resource--open-url (urlspec)
  "Open the URL specified by URLSPEC."
  (when-let ((url (citar-resource--get-url urlspec)))
    (prog1 t (browse-url url))))

;;;; Files and Notes

(defun citar-resource--find-filenames (basename dirs &optional extensions)
  "Return files named BASENAME with EXTENSIONS in DIRS."
  (let (filenames)
    (dolist (dir dirs (nreverse filenames))
      (dolist (ext (or extensions '(nil)))
        (let* ((basename (if ext (concat basename "." ext) basename))
               (filename (expand-file-name basename dir)))
          (when (file-exists-p filename)
            (push filename filenames)))))))

(defun citar-resource--file-finder (key _entry)
  "Find notes named KEY.
Returns files named KEY in `citar-resource-file-paths` having
extensions in `citar-resource-file-extensions`."
  (seq-map
   (lambda (filename)
     '(file ,(abbreviate-file-name filename) . ,filename))
   (citar-resource--find-filenames
    key citar-resource-file-paths citar-resource-file-extensions)))

(defun citar-resource--note-finder (key _entry)
  "Find files named KEY.
Returns files named KEY in `citar-resource-note-paths` having
extensions in `citar-resource-note-extensions`."
  (seq-map
   (lambda (filename)
     '(note ,(abbreviate-file-name filename) . ,filename))
   (citar-resource--find-filenames
    key citar-resource-note-paths citar-resource-note-extensions)))

(defun citar-resource--file-field-finder (_key entry)
  "Find notes and files given in fields of ENTRY."
  (let ((files nil))
    (dolist (field citar-resource-file-fields (nreverse files))
      (when-let ((field-value (citar-get-value field entry)))
        (dolist (parser citar-resource-file-parser-functions)
          (dolist (filename (funcall parser
                                     (or citar-resource-file-paths '(nil))
                                     field-value))
            (when (file-exists-p filename)
              (let ((restype (if (member (file-name-extension filename)
                                         citar-resource-note-extensions)
                                 'note
                               'file)))
                (push `(,restype ,(abbreviate-file-name filename) . ,filename)
                      files)))))))))

(defun citar-resource--file-equiv-p (first second)
  "Test whether FIRST and SECOND are equivalent files."
  (and (stringp first) (stringp second) (string= first second)))

(defun citar-resource--open-file (filename)
  "Open FILENAME if it is a string and it exists."
  (when (and (stringp filename) (file-exists-p filename))
    (prog1 t (funcall citar-resource-file-open-function filename))))

(defun citar-resource--open-note (filename)
  "Open FILENAME if it is a string and it exists."
  ;; TODO Do something useful if note doesn't exist?
  (when (and (stringp filename) (file-exists-p filename))
    (prog1 t (funcall citar-resource-note-open-function filename))))

;;;; Interactive commands and helpers

(defun citar-resource--list (key entry)
  "Return list of resources for KEY and ENTRY."
  (seq-uniq
   (seq-mapcat (lambda (finder) (funcall finder key entry))
               citar-resource-finders)
   (lambda (first second)
     (pcase-let ((`(,type ,str . ,first) first)
                 (`(,othertype ,otherstr . ,second) second))
       (or (string= str otherstr)    ; for now, require unique display strings
           (when-let ((predicates
                       (and (eq type othertype)
                            (alist-get type citar-resource-equiv-predicates))))
             (seq-some (lambda (pred) (funcall pred first second))
                       predicates)))))))

(defun citar-resource-open (key-entry)
  "Open resources for KEY-ENTRY."
  (interactive (list (citar-select-ref :rebuild-cache current-prefix-arg)))
  (let* ((resources (citar-resource--list (car key-entry) (cdr key-entry)))
         (cands (seq-map (lambda (resource) (cons (cadr resource) resource))
                         resources))
         (group-fun (lambda (cand transform)
                      (if transform
                          cand
                        (alist-get (car (cdr (assoc cand cands)))
                                   citar-resource-groups))))
         (selected
          (completing-read
           "Open: "
           (lambda (string pred action)
             (if (eq action 'metadata)
                 `(metadata (category . citar-resource)
                            (group-function . ,group-fun))
               (complete-with-action action cands string pred)))
           nil 'require-match))
         (resource (cdr (assoc selected cands))))
    (seq-some (lambda (opener) (funcall opener (cddr resource)))
              (alist-get (car resource) citar-resource-open-functions))))


(provide 'citar-resource)
;;; citar-resource.el ends here
