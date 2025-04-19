;;; hrm.el --- Consult-powered note taking system -*- lexical-binding: t -*-

;; Author: gmbuell
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (consult "0.16") (marginalia "0.12") (embark "0.16"))
;; Keywords: notes, files, convenience

;;; Commentary:

;; hrm is a flexible note taking system powered by consult, marginalia, and
;; embark. It provides a unified interface to access and manage notes across
;; multiple directories.
;;
;; Key features:
;; - Access notes from multiple configurable sources
;; - Filter notes by source using consult narrowing
;; - Rich annotations showing note metadata
;; - Powerful actions via embark integration
;; - Quick creation of new notes
;; - Full-text search across notes

;;; Code:

(require 'consult)
(require 'marginalia)
(require 'embark)
(require 'project)

;;;; Customization

(defgroup hrm nil
  "Consult-powered note taking."
  :group 'files
  :prefix "hrm-")

(defcustom hrm-notes-category 'hrm-note
  "Category symbol for the notes in this package."
  :type 'symbol)

(defcustom hrm-default-note-extension ".org"
  "Default extension for new notes."
  :type 'string)

(defcustom hrm-notes-sources
  '(("Projects"   ?p "~/notes/projects/"))
  "List of note sources.
Each source is a list of (NAME CHAR DIR) where:
NAME is the source name,
CHAR is the narrowing character for consult,
DIR is the directory to find notes."
  :type '(repeat (list (string :tag "Name")
                       (character :tag "Narrowing char")
                       (directory :tag "Directory"))))

(defcustom hrm-note-mode-function #'org-mode
  "Function to call when opening a note file."
  :type 'function)

(defvar hrm-notes-history nil
  "History variable for `hrm-notes'.")

(defvar hrm-cache nil
  "Cache of note files to improve performance.")

(defvar hrm-cache-last-updated nil
  "Timestamp when the cache was last updated.")

;;;; Internal functions

(defun hrm-ensure-directory (dir)
  "Ensure DIR exists, creating it if necessary."
  (unless (file-exists-p dir)
    (make-directory dir t))
  dir)

(defun hrm-cache-valid-p ()
  "Return non-nil if the cache is still valid."
  (and hrm-cache
       hrm-cache-last-updated
       (< (float-time (time-subtract (current-time) hrm-cache-last-updated)) 30)))

(defun hrm-refresh-cache ()
  "Refresh the cache of note files."
  (setq hrm-cache nil)
  (dolist (source hrm-notes-sources)
    (let* ((name (nth 0 source))
           (dir (nth 2 source))
           (idir (propertize (file-name-as-directory dir) 'invisible t)))
      (when (file-exists-p dir)
        (let ((files (directory-files dir nil "[^.].*[.].+")))
          (setq hrm-cache (append hrm-cache
																	(mapcar (lambda (f)
																						(propertize (concat idir f)
																												'hrm-source name))
																					files)))))))
  (setq hrm-cache-last-updated (current-time)))

(defun hrm-get-all-notes ()
  "Return a list of all note files."
  (unless (hrm-cache-valid-p)
    (hrm-refresh-cache))
  hrm-cache)

(defun hrm-annotate-note (cand)
  "Annotate file CAND with its source name, size, and modification time."
  (let* ((attrs (file-attributes cand))
         (source (get-text-property 0 'hrm-source cand))
         (fsize (file-size-human-readable (file-attribute-size attrs)))
         (ftime (format-time-string "%b %d %H:%M" (file-attribute-modification-time attrs))))
    (put-text-property 0 (length source) 'face 'marginalia-type source)
    (put-text-property 0 (length fsize) 'face 'marginalia-size fsize)
    (put-text-property 0 (length ftime) 'face 'marginalia-date ftime)
    (format "%15s  %7s  %10s" source fsize ftime)))

(defun hrm-notes-make-source (name char dir)
  "Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes."
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,name
								:narrow   ,char
								:category ,hrm-notes-category
								:face     consult-file
								:annotate hrm-annotate-note
								:items    ,(lambda ()
														 (when (file-exists-p dir)
															 (mapcar (lambda (f)
																				 (propertize (concat idir f) 'hrm-source name))
																			 (directory-files dir nil "[^.].*[.].+"))))
								:action   ,(lambda (f)
														 (find-file f)
														 (funcall hrm-note-mode-function)))))

(defun hrm-get-source-directory (source-name)
  "Get directory for SOURCE-NAME."
  (let ((source (seq-find (lambda (s) (string= (car s) source-name))
                          hrm-notes-sources)))
    (when source
      (nth 2 source))))

(defun hrm-parse-title (title)
  "Parse TITLE to extract title and optional source name."
  (if (string-match "\\`\\(.*\\)@\\(.*\\)\\'" title)
      (cons (match-string 1 title) (match-string 2 title))
    (cons title nil)))

;;;; User Interface

;;;###autoload
(defun hrm-notes ()
  "Find a file in a notes directory."
  (interactive)
  (let ((completion-ignore-case t))
    (consult--multi (mapcar (lambda (s) (apply #'hrm-notes-make-source s))
                            hrm-notes-sources)
                    :prompt "Notes File: "
                    :require-match t
                    :sort nil
                    :group nil
                    :history 'hrm-notes-history)))

;;;###autoload
(defun hrm-new-note (title)
  "Create a new note with TITLE.
If TITLE contains @SOURCE, create the note in the SOURCE directory.
Otherwise, create in the first available directory."
  (interactive "sNote title: ")
  (let* ((parsed (hrm-parse-title title))
         (name (car parsed))
         (source-name (cdr parsed))
         (dir (if source-name
                  (hrm-get-source-directory source-name)
                (nth 2 (car hrm-notes-sources))))
         (filename (concat (hrm-ensure-directory dir)
                           (format "%s%s"
																	 (replace-regexp-in-string "\\s-+" "-" name)
																	 hrm-default-note-extension))))
    (find-file filename)
    (when (= (point-min) (point-max))
      (insert (format "#+TITLE: %s\n#+DATE: %s\n\n"
											name (format-time-string "%Y-%m-%d")))
      (goto-char (point-max)))
    (funcall hrm-note-mode-function)))

;;;###autoload
(defun hrm-search-notes ()
  "Search content within notes."
  (interactive)
  (let ((dirs (seq-filter #'file-exists-p
                          (mapcar #'caddr hrm-notes-sources))))
    (consult-ripgrep (if (= (length dirs) 1)
                         (car dirs)
                       (completing-read "Search in: " dirs nil t)))))

;;;###autoload
(defun hrm-capture-dwim ()
  "Capture a note using the current context.
In a project, uses the project name as the note title.
With region active, uses the region as the note content."
  (interactive)
  (let ((title (cond ((use-region-p)
                      (let ((text (buffer-substring-no-properties
                                   (region-beginning) (region-end))))
                        (string-trim (substring text 0 (min 60 (length text))))))
                     ((project-current)
                      (file-name-nondirectory
                       (directory-file-name (project-root (project-current)))))
                     (t
                      (read-string "Note title: ")))))
    (hrm-new-note title)
    (when (use-region-p)
      (let ((content (buffer-substring-no-properties
                      (region-beginning) (region-end))))
        (insert content)))))

;;;; Embark integration

(defun hrm-notes-dired (cand)
  "Open notes directory dired with point on file CAND."
  (interactive "fNote: ")
  (dired-jump nil cand))

(defun hrm-notes-grep (cand)
  "Run consult-ripgrep in directory of notes file CAND."
  (interactive "fNote: ")
  (consult-ripgrep (file-name-directory cand)))

(defun hrm-notes-rename (cand)
  "Rename note file CAND."
  (interactive "fNote: ")
  (let* ((old-name (file-name-nondirectory cand))
         (dir (file-name-directory cand))
         (new-name (read-string "New name: " old-name)))
    (rename-file cand (concat dir new-name) 1)
    (find-file (concat dir new-name))
    (funcall hrm-note-mode-function)))

(defun hrm-notes-marked (candidates)
  "Open a list of note CANDIDATES in a dired buffer."
  (interactive (list (embark-region-active-p)))
  (embark-export-dired candidates))

(defvar-keymap hrm-notes-map
  :doc "Keymap for Embark notes actions."
  :parent embark-file-map
  "d" #'hrm-notes-dired
  "g" #'hrm-notes-grep
  "m" #'hrm-notes-marked
  "r" #'hrm-notes-rename
  "n" #'hrm-new-note)

;;;; Setup

(add-to-list 'embark-keymap-alist `(,hrm-notes-category . hrm-notes-map))
;; make embark-export use dired for notes
(setf (alist-get hrm-notes-category embark-exporters-alist) #'embark-export-dired)

(provide 'hrm)
;;; hrm.el ends here
