;;; symbol-overlay-mc.el --- Mark highlighted symbols with multiple cursors  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://lmno.lol/alvaro
;; URL: https://github.com/xenodium/symbol-overlay-mc
;; Package-Version: 20241216.1436
;; Package-Revision: 188fa07fe5cc
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1") (multiple-cursors "1.4.0") (symbol-overlay "4.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides integration between the `symbol-overlay'
;; package (which will highlight all instances in the current buffer of
;; the symbol currently under the cursor) and the `multiple-cursors'
;; package.  Invoke function `symbol-overlay-mc-mark-all' to place a
;; cursor on every symbol currently highlighted.
;;
;; If you wish to add this to the transient menu provided by the
;; `casual-symbol-overlay' package, add the following to your init file:
;; (with-eval-after-load 'casual-symbol-overlay
;;   (symbol-overlay-mc-insert-into-casual-tmenu)

;;; Code:

(require 'multiple-cursors-core)
(require 'seq)
(require 'symbol-overlay)

;;;###autoload
(defun symbol-overlay-mc-mark-all ()
  "Mark all symbol overlays using multiple cursors."
  (interactive)
  (when-let* ((overlays (symbol-overlay-get-list 0))
              (point (point))
              (point-overlay (seq-find
                              (lambda (overlay)
                                (and (<= (overlay-start overlay) point)
                                     (<= point (overlay-end overlay))))
                              overlays))
              (offset (- point (overlay-start point-overlay))))
    (setq deactivate-mark t)
    (mapc (lambda (overlay)
            (unless (eq overlay point-overlay)
              (mc/save-excursion
               (goto-char (+ (overlay-start overlay) offset))
               (mc/create-fake-cursor-at-point))))
          overlays)
    (mc/maybe-multiple-cursors-mode)))

(add-to-list 'mc--default-cmds-to-run-once 'symbol-overlay-mc-mark-all t)

;;;###autoload
(defun symbol-overlay-mc-insert-into-casual-tmenu ()
  (interactive)
  (require 'casual-symbol-overlay
  (transient-append-suffix 'casual-symbol-overlay-tmenu '(-2)
    ["Multiple cursors"
     ("c" "Mark all" symbol-overlay-mc-mark-all)])))

(provide 'symbol-overlay-mc)

;;; symbol-overlay-mc.el ends here
