;;; orb-book.el --- Book manager empowered by org-roam-bibtex  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Junwei Wang
;;
;; Author: Junwei Wang <https://github.com/junwei-wang>
;; Maintainer: Junwei Wang <junwei.wang@cryptoexperts.com>
;; Created: September 24, 2021
;; Modified: September 24, 2021
;; Version: 0.0.1
;; Keywords: bib book
;; Homepage: https://github.com/junwei-wang/orb-book
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The API is inspired by calibredb <https://github.com/chenyanming/calibredb.el>.
;;
;;  Description:
;;
;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'helm)
(require 'org-roam-bibtex)

;; ============================================================================
;;;; Customize definitions
;; ============================================================================

(defcustom orb-book-helm-actions
  (if (fboundp 'helm-make-actions)
      (helm-make-actions
       "Open PDF file"               'orb-book-open-pdf
       "Attach PDF file"             'orb-book-add-pdf
       "View details"                'orb-book-show-entry))
  "Default actions for orb-book helm."
  :group 'orb-book
  :type '(alist :key-type string :value-type function))

;; ============================================================================
;;;; Orb book query books, and process them
;; ============================================================================

(defun orb-book--all-refs ()
  "Get the references for all org-roam nodes for books.
A book org-roam node is a node generated dy org-roam-bibtex and having a tag
\"book\" or \"BOOK\" or \"Book\"."
  (org-roam-db-query [:select [ref]
                      :from tags
                      :join refs :on (= tags:node_id refs:node_id)
                      :where (in tag ["book" "BOOK" "Book"])]))

(defun orb-book-query ()
  "Query books' information from bibtex."
  (mapcar
   (lambda(ref)
     (let* ((ref (car ref))
            (bib-entry (bibtex-completion-get-entry ref))
            (title (bibtex-completion-get-value "title" bib-entry))
            (long-authors (or (bibtex-completion-get-value "author" bib-entry)
                         (bibtex-completion-get-value "editor" bib-entry)))
            (short-authors (bibtex-completion-shorten-authors long-authors))
            (publisher (bibtex-completion-get-value "publisher" bib-entry))
            (year (or (bibtex-completion-get-value "year" bib-entry)
                      (car (split-string (bibtex-completion-get-value "date" bib-entry "") "-"))))

            (raw-edition (bibtex-completion-get-value "edition" bib-entry))
            (edition (cond
                      ((numberp raw-edition) raw-edition)
                      ((stringp raw-edition)
                       (cond ((string= "Second" raw-edition) 2)
                             ((string-match "[1-9]" raw-edition)
                              (string-to-number
                               (match-string (string-match "[1-9]" raw-edition) raw-edition)))
                             (t raw-edition "x")))
                      (t raw-edition)))
            (isbn (bibtex-completion-get-value "isbn" bib-entry))
            (series (bibtex-completion-get-value "series" bib-entry))
            (url (bibtex-completion-get-value "url" bib-entry)))
       (list ref title long-authors short-authors year publisher edition series isbn url)))
   (orb-book--all-refs)))

(defun orb-book-query-to-alist (item)
  "Builds alist out of a full `orb-book-query' query record result.
ITEM is an entry in the query result."
  (if item
    `((:ref           ,(nth 0 item))
      (:title         ,(nth 1 item))
      (:authors       ,(nth 2 item))
      (:short-authors ,(nth 3 item))
      (:year          ,(nth 4 item))
      (:publisher     ,(nth 5 item))
      (:edition       ,(nth 6 item))
      (:series        ,(nth 7 item))
      (:isbn          ,(nth 8 item))
      (:url           ,(nth 9 item)))))

(defun orb-book-format-column (string width &optional right-align)
  "Return STRING truncated or padded to WIDTH.
Argument RIGHT-ALIGN."
  (cond ((< width 0) string)
        ((= width 0) "")
        (t (format (format "%%%s%d.%ds" (if right-align "" "-") width width)
                   string))))

(defun orb-book-getattr (my-alist key)
  "Get the associated attribute for KEY in MY-ALIST."
  (cadr (assoc key my-alist)))

(defun orb-book-format-item (book-alist)
  "Format the candidate string shown in helm.
Argument BOOK-ALIST."
  (let ((title (orb-book-getattr book-alist :title))
        (short-authors (orb-book-getattr book-alist :short-authors))
        (year (orb-book-getattr book-alist :year))
        (edition (orb-book-getattr book-alist :edition))
        (publisher (orb-book-getattr book-alist :publisher)))
    (format
     "%s  %s  %s  %s  %s"
     (orb-book-format-column short-authors 20)
     (orb-book-format-column year 4 t)
     (orb-book-format-column title 80)
     (orb-book-format-column edition 2 t)
     (orb-book-format-column publisher 30))))

(defun orb-book-get-list-for-display (item-list)
  "Get book list for display in helm from orb-book ITEM-LIST."
  (mapcar (lambda(item) (list (orb-book-format-item item) item)) item-list))

(defun orb-book-candidates()
  "Generate books candidates alist."
  (let* ((query-result (orb-book-query)))
    (cond ((equal nil query-result) '(""))
          (t (orb-book-get-list-for-display
              (mapcar 'orb-book-query-to-alist query-result))))))

;; ============================================================================
;;;; Orb book helm actions
;; ============================================================================

(defun orb-book-open-pdf (entry)
  "Open associated pdf file for the book ENTRY."
  (let ((ref (orb-book-getattr (car entry) :ref)))
    (bibtex-completion-open-pdf (list ref))))

(defun orb-book-add-pdf (entry)
  "Open associated pdf file for the book ENTRY."
  (let ((ref (orb-book-getattr (car entry) :ref)))
    (bibtex-completion-add-pdf-to-library (list ref))))

;; ============================================================================
;;;; Orb book helm search
(defun orb-book-helm-read ()
  "Helm read for orb-book."
  (when (fboundp 'helm)
    (when (get-buffer "*helm action*")
      (kill-buffer "*helm action*"))
    (unwind-protect
        (helm :sources (if (fboundp 'helm-build-sync-source)
                           (helm-build-sync-source "Book list"
                             :candidates (orb-book-candidates)
                             :action 'orb-book-helm-actions))
              :buffer "*helm orb-book*") )))

(defun orb-book-find-helm ()
  "Use helm to list all book details."
  (interactive)
  (orb-book-helm-read))

(provide 'orb-book)
;;; orb-book.el ends here
; Local Variables:
; fill-column: 79
; End:
