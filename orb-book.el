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

(defcustom orb-book-bibliography nil
  "Bib file for inserting new books by orb-book.
This file should be added to variable `bibtex-completion-bibliography'"
  :group 'orb-book
  :type '(string))

;; ============================================================================
;;;; Orb book query books, and process them
;; ============================================================================

(defun orb-book--all-refs ()
  "Get the references for all org-roam nodes for books.
A book org-roam node is a node generated dy org-roam-bibtex and having a tag
\"book\" or \"BOOK\" or \"Book\"."
  (org-roam-db-query [:select [tags:node_id, ref]
                      :from tags
                      :join refs :on (= tags:node_id refs:node_id)
                      :where (in tag ["book" "BOOK" "Book"])]))

(defun orb-book-query-book-tags (node-id)
  "Query the tags of a book with NODE-ID."
  (mapcar 'car
          (org-roam-db-query [:select tag :from tags
                              :where (= node_id $s1)
                              :and (not-in tag ["book" "BOOK" "Book"])]
                             node-id)))

(defun orb-book-query ()
  "Query books' information from bibtex."
  (mapcar
   (lambda(entry)
     (let* ((node-id (nth 0 entry))
            (ref (nth 1 entry))
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
                               (match-string (string-match "[1-9][0-9]*" raw-edition) raw-edition)))
                             (t raw-edition)))
                      (t raw-edition)))
            (isbn (bibtex-completion-get-value "isbn" bib-entry))
            (series (bibtex-completion-get-value "series" bib-entry))
            (url (bibtex-completion-get-value "url" bib-entry))
            (has-pdf (bibtex-completion-get-value "=has-pdf=" bib-entry))
            (tags (orb-book-query-book-tags node-id)))
       (list ref title long-authors short-authors year publisher edition series isbn url has-pdf tags)))
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
      (:url           ,(nth 9 item))
      (:has-pdf       ,(nth 10 item))
      (:tags          ,(nth 11 item)))))

(defun orb-book-format-column (string width &optional right-align)
  "Return STRING truncated or padded to WIDTH.
Argument RIGHT-ALIGN."
  (cond ((< width 0) string)
        ((= width 0) "")
        (t (format (format "%%%s%d.%ds" (if right-align "" "-") width width)
                   (or string "")))))

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
        (publisher (orb-book-getattr book-alist :publisher))
        (has-pdf (orb-book-getattr book-alist :has-pdf))
        (tags (orb-book-getattr book-alist :tags)))
    (format
     "%s  %s  %s  %s  %s %s %s"
     (orb-book-format-column has-pdf 1)
     (orb-book-format-column short-authors 20)
     (orb-book-format-column year 4 t)
     (orb-book-format-column title 80)
     (orb-book-format-column edition 4 t)
     (orb-book-format-column publisher 30)
     (orb-book-format-column tags 30))))

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
;; ============================================================================

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

;; ============================================================================
;;;; Add new book
;; ============================================================================


(defun orb-book-copy-add-book-buffer ()
  "Copy the new book entry to the corresponding bib file."
  (interactive)
  ;; kill all comments, then format the buffer,
  ;; then append the bib entry to bib file
  (goto-char 0)
  (comment-kill (count-lines (point-min) (point-max)))
  (bibtex-reformat)
  (append-to-file (point-min) (point-max) orb-book-bibliography)
  (message "book entry has been added to `%s'" orb-book-bibliography)
  (kill-current-buffer)

  ;; TODO get the ref key of the bib entry
  ;;
  ;; TODO test whether note already exist
  ;; (let ((bib-buffer-name (buffer-name)))
  ;;   (save-excursion
  ;;     ;; create org-roam-bibtex node
  ;;     ;; FIXME: no ROAM_REFS is created
  ;;     (orb-bibtex-completion-edit-note
  ;;      (list (read-string "Enter the bib ref key (e.g. \"BOOK:Knuth1997:AOCP2\"): ")))
  ;;    (kill-buffer bib-buffer-name)
  ;;    )
  )

(defun orb-book-add-book-quit ()
  "Quit the buffer in `orb-book-add-book-mode'."
  (interactive)
  (kill-current-buffer))


(defvar orb-book-add-book-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-k" 'orb-book-add-book-quit)
    (define-key map "\C-c\C-c" 'orb-book-copy-add-book-buffer)
    map)
  "Keymap for `orb-book-add-book-mode'.")

(define-derived-mode orb-book-add-book-mode
  bibtex-mode "orb-book add book"
  "Major mode for org-book add book.")

(defun orb-book-add-book ()
  "Add a new book to orb-book."
  (interactive)
  (unless orb-book-bibliography
    (error "`orb-book-bibliography' is not set"))
  (switch-to-buffer (make-temp-name "*orb-book-new-book*"))
  (insert "\n\n"
          "@Comment please insert your bibtex bib entry above.\n"
          "@Comment\n"
          "@Comment press C-c C-c to finish\n"
          "@Comment press C-c C-k to quit\n"
          "@Comment\n"
          "@Comment for example:\n"
          "@Comment\n"
          "@Comment @book{BOOK:Knuth1997:AOCP2,\n"
          "@Comment title =     {The art of computer programming. Vol.2. Seminumerical algorithms},\n"
          "@Comment author =    {Knuth, Donald E},\n"
          "@Comment publisher = {Addison-Wesley Professional},\n"
          "@Comment isbn =      {0-201-89684-2,978-0-201-89684-8,978-0-201-89683-1,0201896834,978-0-201-89685-5,0201896850,978-0-201-03804-0},\n"
          "@Comment year =      {1997},\n"
          "@Comment series =    {},\n"
          "@Comment edition =   {3ed.},\n"
          "@Comment volume =    {},\n"
          "@Comment url =       {https://www-cs-faculty.stanford.edu/~knuth/taocp.html}\n"
          "@Comment }\n")
  (goto-char 0)
  (orb-book-add-book-mode))

(provide 'orb-book)
;;; orb-book.el ends here
; Local Variables:
; fill-column: 79
; End:
