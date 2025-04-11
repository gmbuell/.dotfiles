;;; palaver.el --- Window management with drawers -*- lexical-binding: t -*-

;; Author: gmbuell
;; Version: 1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1") (seq "2.20") (vertico-multiform "2.0") (vertico-buffer "2.0") (consult "2.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a comprehensive window management system
;; built around the concept of "drawers" - specialized windows that
;; contain ephemeral or auxiliary buffers like help, compilation output,
;; or REPLs.
;;
;; The main layout consists of:
;; - One or two "main windows" side-by-side for editing files
;;   (automatically expanded to at least 82 characters wide when selected)
;; - A bottom drawer for shells, compilation, etc.
;; - A right drawer for help, documentation, etc.
;;
;; Key features:
;; - Buffer management based on rules
;; - Project-aware drawer organization
;; - Automatic main window width adjustment
;; - Integration with Emacs 29 window management
;; - Context-aware buffer switching

;;; Basic Usage:

;; Add to your init.el:
;;   (use-package palaver
;;     :ensure nil ; if installing locally
;;     :demand t ; Load immediately, no lazy loading
;;     :init
;;     (setq palaver-main-window-min-width 82)
;;     (palaver-mode 1) ; Enable directly in :init
;;     :bind
;;     (("C-c k" . palaver-toggle-bottom-drawer)
;;      ("C-c l" . palaver-toggle-right-drawer)
;;      ("C-c m" . palaver-toggle-drawer-location)
;;      ("C-x o" . palaver-other-window)))
;;
;; When a drawer is open, C-x b will only show other drawer buffers
;; When in a main window, C-x b will only show regular file buffers

;;; Code:

;; Enable winner-mode for window configuration undo/redo
(with-eval-after-load 'winner
  (winner-mode 1))

;; Required libraries
(require 'seq)
(require 'vertico-multiform)
(require 'vertico-buffer)
(require 'consult)

;;; --------------------------------------------------------
;;; Customization and Variables
;;; --------------------------------------------------------

(defgroup palaver nil
  "Window management with drawers."
  :group 'convenience)

(defcustom palaver-bottom-drawer-height 0.3
  "Height of bottom drawer (proportion of frame).
Values between 0 and 1 represent a fraction of the frame height."
  :type 'float
  :group 'palaver)

(defcustom palaver-right-drawer-width 80
  "Width of right drawer (in characters)."
  :type 'integer
  :group 'palaver)

(defcustom palaver-drawer-width-threshold 160
  "Width threshold for drawer placement.
If frame width is greater than this value, special buffers will
default to the right drawer instead of the bottom drawer.
This helps ensure optimal layout on different screen sizes."
  :type 'integer
  :group 'palaver)

(defcustom palaver-main-window-min-width 80
  "Minimum width in characters for main windows.
If a main window is narrower than this when selected, it will be
automatically expanded to this width."
  :type 'integer
  :group 'palaver)

;; Appearance customization options
(defcustom palaver-drawer-mode-line '(:eval (propertize " DRAWER" 'face 'mode-line-emphasis))
  "String or sexp to show in the mode-line of drawer buffers.
Can be a quoted list or function. Setting this to nil removes
the mode-line indicator from drawer buffers."
  :type '(choice (const :tag "Off" nil)
                 (string :tag "Literal text")
                 (sexp :tag "General `mode-line-format' entry"))
  :group 'palaver)

(defcustom palaver-drawer-mode-line-position 0
  "Position in mode-line to place drawer indicator."
  :type 'integer
  :group 'palaver)

;; Project grouping function
(defcustom palaver-group-function 'palaver-group-by-project
  "Function that returns a drawer context.
When set to nil, drawers are not grouped by context.

This function is called with no arguments and should return a
string or symbol identifying a drawer buffer's group. This
identifier is used to associate drawers with regular buffers
\(by project, directory or major-mode\) so that drawers
are restricted to their associated group.

Built-in choices include:
`palaver-group-by-directory': Return project root or default directory
`palaver-group-by-project': Return project root using project.el"
  :type '(choice
          (const :tag "Don't group drawers" nil)
          (const :tag "Group by project" palaver-group-by-project)
          (const :tag "Group by directory" palaver-group-by-directory)
          (function :tag "Custom function"))
  :group 'palaver)

;; Internal state variables
(defvar palaver-last-active-main-window nil
  "Last focused main window.")

(defvar palaver-bottom-drawer-open nil
  "Whether the bottom drawer is currently open.")

(defvar palaver-right-drawer-open nil
  "Whether the right drawer is currently open.")

;; Project-aware MRU buffer lists
(defvar palaver-drawer-buffers-by-project (make-hash-table :test 'equal)
  "Hash table mapping project roots to (bottom-buffers . right-buffers) pairs.")

;; Buffer-local variables for drawer status tracking
(defvar-local palaver-drawer-status nil
  "Identifies a buffer's drawer status with its buffer-local value.
Valid values are:
'drawer     : Buffer is a drawer buffer determined by drawer rules
nil         : Regular buffer")

(defvar-local palaver-preferred-drawer-side nil
  "The preferred drawer side for this buffer.
Possible values are 'bottom, 'right, or nil.")

;; Rules for identifying drawer buffers
(defcustom palaver-drawer-rules
  '(;; Major modes for drawer windows
    (compilation-mode :drawer t :side bottom)
    (help-mode :drawer t :side right)
    (helpful-mode :drawer t :side right)
    (eshell-mode :drawer t :side bottom)
    (shell-mode :drawer t :side bottom)
    (term-mode :drawer t :side bottom)
    (vterm-mode :drawer t :side bottom)
    (comint-mode :drawer t :side bottom)
    (grep-mode :drawer t :side right)
    (occur-mode :drawer t :side right)
    (magit-mode :drawer t :side right)
    ;; (dired-mode :drawer t :drawer-if pop-up)
    (ibuffer-mode :drawer t :side right)
    (Info-mode :drawer t :side right)
    (debugger-mode :drawer t :side bottom)
    (special-mode :drawer t)
    (completion-list-mode :drawer t :side bottom)
    (Buffer-menu-mode :drawer t :side right)
    (package-menu-mode :drawer t :side right)
    (process-menu-mode :drawer t :side bottom)
    (tabulated-list-mode :drawer t)
    (imenu-list-major-mode :drawer t :side right)
    (embark-collect-mode :drawer t :side right)
    (org-agenda-mode :drawer t :side right)
    (calendar-mode :drawer t :side right)

    ;; Buffer name patterns (regex)
    ("\\*Help\\*" :drawer t :side right :regexp t)
    ("\\*Completions\\*" :drawer t :side bottom :regexp t)
    ("\\*Backtrace\\*" :drawer t :side bottom :regexp t)
    ("\\*compilation\\*" :drawer t :side bottom :regexp t)
    ("\\*Compile-Log\\*" :drawer t :side bottom :regexp t)
    ("\\*Warnings\\*" :drawer t :side bottom :regexp t)
    ("\\*Messages\\*" :drawer t :side bottom :regexp t)
    ("\\*scratch\\*" :drawer t :regexp t)
    ("\\*eldoc\\*" :drawer t :side right :regexp t)
    ("\\*grep\\*" :drawer t :side right :regexp t)
    ("\\*Occur\\*" :drawer t :side right :regexp t)
    ("\\*Shell Command Output\\*" :drawer t :side bottom :regexp t)
    ("\\*Async Shell Command\\*" :drawer t :side bottom :regexp t)
    ("^magit" :drawer t :side right :regexp t)
    ("^\\*eshell" :drawer t :side bottom :regexp t)
    ("\\*imenu-list\\*" :drawer t :side right :regexp t)
    ("\\*Embark .*\\*" :drawer t :side right :regexp t)
    ("\\*Embark Collect.*\\*" :drawer t :side right :regexp t)
    ("\\*Embark Export.*\\*" :drawer t :side right :regexp t)
    ("\\*Flymake .*\\*" :drawer t :side bottom :regexp t)
    ("\\*Flycheck errors\\*" :drawer t :side bottom :regexp t)
    ("\\*xref\\*" :drawer t :side right :regexp t)

    ;; Custom function
    (:pred palaver-side-window-buffer-p :drawer t))
  "ALIST of rules for what buffers should be displayed in drawer windows.
Each rule can be:
- A major mode symbol
- A string (buffer name or regex pattern with :regexp t)
- A cons of (:pred function) where function takes a buffer and returns non-nil if it should be in a drawer

Each rule has properties including:
- :drawer - Whether this buffer should be in a drawer
- :side - Which drawer to prefer (bottom or right)
- :regexp - Whether string patterns should be treated as regex
- :suppress - If non-nil, don't display this drawer automatically"
  :type '(repeat (choice (list symbol (plist :inline t))
                         (list string (plist :inline t))
                         (list (list :format "%v"
                                     (const :format "%t: " :pred)
                                     function)
                               (plist :inline t))))
  :group 'palaver)

;;; --------------------------------------------------------
;;; Helper functions for drawer buffer management
;;; --------------------------------------------------------

(defun palaver-get-project-root ()
  "Get current project root or a default identifier."
  (or (and palaver-group-function (funcall palaver-group-function))
      "global"))

(defun palaver-get-drawer-buffers (side)
  "Get MRU buffer list for SIDE drawer in current project."
  (let* ((project-root (palaver-get-project-root))
         (project-drawers (gethash project-root palaver-drawer-buffers-by-project nil))
         (drawer-buffers (if (eq side 'bottom)
                             (car project-drawers)
                           (cdr project-drawers))))
    ;; Filter out dead buffers
    (setq drawer-buffers (seq-filter #'buffer-live-p drawer-buffers))
    ;; Update the hash table with cleaned list
    (palaver-set-drawer-buffers side drawer-buffers)
    drawer-buffers))

(defun palaver-set-drawer-buffers (side buffers)
  "Set MRU BUFFERS list for SIDE drawer in current project."
  (let* ((project-root (palaver-get-project-root))
         (project-drawers (gethash project-root palaver-drawer-buffers-by-project nil))
         (bottom-buffers (if (eq side 'bottom) buffers (car project-drawers)))
         (right-buffers (if (eq side 'right) buffers (cdr project-drawers))))
    (puthash project-root (cons bottom-buffers right-buffers)
             palaver-drawer-buffers-by-project)))

(defun palaver-buffer-is-minibuffer-or-vertico-p (buffer)
  "Return t if BUFFER is a minibuffer or vertico buffer."
  (when (buffer-live-p buffer)
    (or (with-current-buffer buffer
          (derived-mode-p 'minibuffer-mode))
        (string-prefix-p " *Vertico" (buffer-name buffer)))))

(defun palaver-add-drawer-buffer (side buffer)
  "Add BUFFER to front of MRU list for SIDE drawer in current project.
Ignores minibuffer and vertico buffers."
  (when (and side buffer (buffer-live-p buffer)
             (not (palaver-buffer-is-minibuffer-or-vertico-p buffer)))
    (let ((buffers (palaver-get-drawer-buffers side)))
      ;; Remove buffer if already in list
      (setq buffers (seq-remove (lambda (b) (eq b buffer)) buffers))
      ;; Add to front of list
      (palaver-set-drawer-buffers side (cons buffer buffers)))))

(defun palaver-get-most-recent-drawer-buffer (side)
  "Get most recent buffer for SIDE drawer in current project."
  (car (palaver-get-drawer-buffers side)))

(defun palaver-remove-from-drawer-buffer (side buffer)
  "Remove BUFFER from the MRU list for SIDE drawer in current project."
  (when (and side buffer (buffer-live-p buffer))
    (let ((buffers (palaver-get-drawer-buffers side)))
      ;; Remove buffer from list
      (setq buffers (seq-remove (lambda (b) (eq b buffer)) buffers))
      ;; Update the drawer buffers
      (palaver-set-drawer-buffers side buffers))))

;;; --------------------------------------------------------
;;; Group Functions
;;; --------------------------------------------------------

(defun palaver-group-by-directory ()
  "Return an identifier (default directory) to group drawers.
The project root is used if found, with default directory as fallback."
  (or (and (fboundp 'project-root)
           (when-let ((project (project-current)))
             (project-root project)))
      (expand-file-name default-directory)))

(defun palaver-group-by-project ()
  "Return an identifier (project root) to group drawers."
  (when (fboundp 'project-root)
    (when-let ((project (project-current)))
      (project-root project))))

;;; --------------------------------------------------------
;;; Core Functions - Drawer Detection and Buffer Classification
;;; --------------------------------------------------------

(defun palaver-side-window-buffer-p (buffer)
  "Custom predicate to determine if BUFFER should be in a side window.
Checks if buffer is already displayed in a side window."
  (let ((window (get-buffer-window buffer)))
    (and window (window-parameter window 'window-side))))

(defun palaver-match-drawer-rule (buffer rule)
  "Check if BUFFER matches drawer RULE and return properties if it does."
  (let ((condition (car rule))
        (props (cdr rule)))
    (when (cond
           ;; Symbol - match major mode
           ((symbolp condition)
            (with-current-buffer buffer
              (derived-mode-p condition)))

           ;; String - match buffer name with or without regexp
           ((stringp condition)
            (let ((name (buffer-name buffer)))
              (if (plist-get props :regexp)
                  (string-match-p condition name)
                (string= condition name))))

           ;; Predicate function
           ((and (listp condition) (eq (car condition) :pred))
            (funcall (cadr condition) buffer)))
      props)))

(defun palaver-buffer-is-drawer-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME should be displayed in a drawer window.
If the buffer matches a rule, returns properties including which drawer it
should appear in. Otherwise returns nil."
  (let* ((buffer (get-buffer buffer-or-name))
         (result nil))
    (when buffer
      (with-current-buffer buffer
        ;; Check for explicit drawer status
        (if palaver-drawer-status
            (progn
              (setq result '(:drawer t))
              ;; Add the preferred side if it's set
              (when palaver-preferred-drawer-side
                (plist-put result :side palaver-preferred-drawer-side)))
          ;; Otherwise, check against rules
          (catch 'found
            (dolist (rule palaver-drawer-rules)
              (let ((props (palaver-match-drawer-rule buffer rule)))
                (when props
                  (setq result props)
                  (setq palaver-drawer-status 'drawer)
                  ;; Store the preferred side if specified
                  (when-let ((side (plist-get props :side)))
                    (setq palaver-preferred-drawer-side side))
                  (throw 'found props))))))))
    result))

(defun palaver-buffer-drawer-side (buffer-or-name)
  "Determine which drawer side BUFFER-OR-NAME should use.
Returns 'bottom, 'right, or nil if not a drawer buffer."
  (let* ((buf (get-buffer buffer-or-name))
         ;; Check for buffer-local preferred side first
         (buffer-local-side (and buf
                                 (buffer-local-value 'palaver-preferred-drawer-side buf)))
         (props (palaver-buffer-is-drawer-p buf))
         (specified-side (and props (plist-get props :side))))
    (cond
     ;; Buffer-local preference takes highest priority
     (buffer-local-side buffer-local-side)

     ;; Rule specifies a side
     (specified-side specified-side)

     ;; Use default logic based on buffer and frame size
     (props
      (if (with-current-buffer buf
            (eq major-mode 'eshell-mode))
          'bottom
        (if (>= (frame-width) palaver-drawer-width-threshold)
            'right
          'bottom)))

     ;; Not a drawer buffer
     (t nil))))

;;; --------------------------------------------------------
;;; Window Management Functions
;;; --------------------------------------------------------

(defun palaver-window-is-side-p (window side)
  "Return non-nil if WINDOW is a side window on SIDE."
  (let ((window-side (window-parameter window 'window-side)))
    (eq window-side side)))

(defun palaver-get-side-window (side)
  "Get a side window on SIDE if it exists."
  (car (seq-filter (lambda (window) (palaver-window-is-side-p window side))
                   (window-list))))

(defun palaver-is-main-window-p (window)
  "Return t if WINDOW is a main window (not a side or minibuffer window)."
  (and (window-live-p window)
       (not (window-parameter window 'window-side))
       (not (window-minibuffer-p window))))

(defun palaver-get-main-windows ()
  "Get a list of all main windows."
  (seq-filter #'palaver-is-main-window-p (window-list)))

(defun palaver-modified-mode-line ()
  "Return modified mode-line with drawer indicator."
  (when palaver-drawer-mode-line
    (if (consp mode-line-format)
        (if (member palaver-drawer-mode-line mode-line-format)
            mode-line-format
          (append
           (seq-take (default-value 'mode-line-format) palaver-drawer-mode-line-position)
           (list palaver-drawer-mode-line
                 (nthcdr palaver-drawer-mode-line-position
                         (default-value 'mode-line-format)))))
      mode-line-format)))

;;; --------------------------------------------------------
;;; Main Window Functions
;;; --------------------------------------------------------

;; Automatically enforce width when switching windows
(defun palaver-other-window-width-advice (&rest _)
  "Advice for enforcing width after `other-window` call."
  (when (palaver-is-main-window-p (selected-window))
    (run-with-idle-timer 0 nil 'palaver-enforce-main-window-width (selected-window))))

(advice-add 'other-window :after #'palaver-other-window-width-advice)

(defun palaver-quit-window-advice (orig-fun &optional kill window)
  "Advice for `quit-window` to properly handle drawer buffers.
ORIG-FUN is the original `quit-window` function.
KILL and WINDOW are passed to the original function."
  (let* ((win (or window (selected-window)))
         (side (window-parameter win 'window-side))
         (buf (window-buffer win)))

    ;; If this is a drawer window, remove the buffer from MRU list
    (when side
      (palaver-remove-from-drawer-buffer side buf))

    ;; Call the original function
    (funcall orig-fun kill window)

    ;; If we just closed the last drawer buffer, close the drawer
    (when (and side
               (window-live-p win)
               (or kill
                   ;; If we're not killing but switching buffers,
                   ;; check if we switched to another drawer buffer
                   (not (palaver-buffer-is-drawer-p (window-buffer win)))))
      (palaver-hide-side-windows side))))

(advice-add 'quit-window :around #'palaver-quit-window-advice)

(defun palaver-clean-killed-drawer-buffer ()
  "Remove a buffer from drawer MRU lists when it's killed."
  (when palaver-drawer-status
    ;; Remove from both drawer MRU lists just to be safe
    (palaver-remove-from-drawer-buffer 'bottom (current-buffer))
    (palaver-remove-from-drawer-buffer 'right (current-buffer))))

(add-hook 'kill-buffer-hook #'palaver-clean-killed-drawer-buffer)

(defun palaver-manage-line-truncation ()
  "Set line truncation in main windows based on their width.
Ensures narrow windows use truncated lines instead of wrapping."
  (let ((main-windows (palaver-get-main-windows)))
    (dolist (window main-windows)
      (let ((is-narrow (< (window-width window) palaver-main-window-min-width)))
        (with-current-buffer (window-buffer window)
          ;; For narrow windows, ensure lines are truncated
          (set-window-parameter window 'truncate-lines (or is-narrow truncate-lines))
          ;; Set buffer-local truncate-lines for consistency
          (with-selected-window window
            (setq-local truncate-lines (or is-narrow truncate-lines))))))))

(defun palaver-enforce-main-window-width (window)
  "Ensure that main WINDOW is at least `palaver-main-window-min-width' wide.
This is called when a main window is selected."
  (when (and (window-live-p window)
             (palaver-is-main-window-p window)
             (< (window-width window) palaver-main-window-min-width))
    (let* ((main-windows (palaver-get-main-windows))
           (other-windows (seq-remove (lambda (w) (eq w window)) main-windows))
           (width-deficit (- palaver-main-window-min-width (window-width window))))

      (when other-windows
        ;; First try: Use balance-windows approach
        (window-resize window width-deficit t nil t)

        ;; Check if we succeeded
        (when (< (window-width window) palaver-main-window-min-width)
          ;; Second try: Be more aggressive with resizing adjacent windows
          (let ((other-window (car other-windows)))
            (condition-case err
                (window-resize other-window (- width-deficit) t)
              (error
               (message "Error resizing adjacent window: %s" (error-message-string err))))))

        ;; Final check and fix
        (when (< (window-width window) palaver-main-window-min-width)
          (balance-windows)
          (window-resize window (- palaver-main-window-min-width (window-width window)) t nil t))))))

(defun palaver-enforce-side-by-side-layout ()
  "Enforce that main windows are always side by side, never stacked.
If windows are vertically stacked, rearranges them horizontally."
  (let ((main-windows (palaver-get-main-windows)))
    (when (>= (length main-windows) 2)
      ;; Sort windows by their position (left to right)
      (setq main-windows
            (sort main-windows
                  (lambda (a b)
                    (< (car (window-edges a)) (car (window-edges b))))))

      ;; Check if any windows are stacked vertically
      (let ((stacked-windows nil))
        (dolist (i (number-sequence 0 (1- (length main-windows))))
          (let* ((win1 (nth i main-windows))
                 (win2 (and (< (1+ i) (length main-windows))
                            (nth (1+ i) main-windows)))
                 (win1-edges (and win1 (window-edges win1))))
            (when (and win1 win2
                       (= (nth 0 win1-edges) (nth 0 (window-edges win2)))  ; Same left edge
                       (window-live-p win1) (window-live-p win2))
              (push (cons win1 win2) stacked-windows))))

        ;; Fix any stacked windows
        (dolist (pair stacked-windows)
          (let* ((win1 (car pair))
                 (win2 (cdr pair))
                 (buf2 (window-buffer win2)))
            (delete-window win2)
            (let ((new-win (split-window win1 nil 'right)))
              (set-window-buffer new-win buf2))))))))

;;; --------------------------------------------------------
;;; Drawer Window Display Functions
;;; --------------------------------------------------------

(defun palaver-enforce-right-drawer-width ()
  "Ensure right drawer maintains its minimum width."
  (when-let ((right-window (palaver-get-side-window 'right)))
    (let ((current-width (window-width right-window))
          (target-width palaver-right-drawer-width))
      (when (< current-width target-width)
        (condition-case resize-err
            (progn
              ;; Try using adjust-window-trailing-edge first
              (adjust-window-trailing-edge right-window (- target-width current-width) t)

              ;; Verify if the adjustment worked
              (when (< (window-width right-window) target-width)
                ;; If not, try more aggressive approach
                (window-resize right-window (- target-width (window-width right-window)) t)

                ;; Final verification
                (when (< (window-width right-window) target-width)
                  nil)))
          (error
           ;; Fallback approach with error logging
           (condition-case err
               (window-resize right-window (- target-width current-width) t)
             (error
              (message "Failed to resize right drawer window: %s"
                       (error-message-string err))))))))))

;; Handle mouse clicks to enforce drawer widths
(defun palaver-handle-mouse-click (event)
  "Handle mouse click EVENT to enforce drawer widths."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (side (and window (window-parameter window 'window-side))))
    (when (eq side 'right)
      (run-with-idle-timer 0 nil 'palaver-enforce-right-drawer-width))))

(global-set-key [mouse-1] 'palaver-handle-mouse-click)

(defun palaver-display-in-side-window (buffer alist side)
  "Display BUFFER in a side window on SIDE.
ALIST is passed to 'display-buffer-in-side-window' internally."
  (let* ((is-right (eq side 'right))
         (is-bottom (eq side 'bottom))
         (params (append alist
                         `((side . ,side)
                           (slot . 0)
                           (window-width . ,(when is-right palaver-right-drawer-width))
                           (window-height . ,(when is-bottom palaver-bottom-drawer-height))
                           (dedicated . side)
                           (preserve-size . ,(cons is-right is-bottom))
                           (window-parameters .
                                              ((no-other-window . t)
                                               (no-delete-other-windows . t)
                                               (mode-line-format . ,(if is-bottom mode-line-format nil))
                                               (quit-restore . (window nil nil nil))
                                               ;; New in Emacs 29: set default face to borderless
                                               (face-mode-line-inactive . mode-line-inactive)
                                               (face-header-line . header-line)))))))
    ;; First, check if buffer is already in a window on the wrong side
    (let ((existing-window (get-buffer-window buffer)))
      (when (and existing-window
                 (window-live-p existing-window)
                 (not (eq side (window-parameter existing-window 'window-side))))
        ;; If the buffer is displayed in the wrong drawer, close that window
        (delete-window existing-window)))

    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq palaver-drawer-status 'drawer)
        ;; Set the preferred drawer side
        (setq-local palaver-preferred-drawer-side side)
        ;; Create modified mode-line
        (when palaver-drawer-mode-line
          (setq mode-line-format (palaver-modified-mode-line)))))

    ;; Display the buffer in side window
    (let ((window (display-buffer-in-side-window buffer params)))
      ;; Record this buffer as the most recent for this drawer side
      (palaver-add-drawer-buffer side buffer)

      ;; Update drawer state
      (if (eq side 'bottom)
          (setq palaver-bottom-drawer-open t)
        (setq palaver-right-drawer-open t))

      ;; If it's a right drawer, ensure it has the proper width
      (when (and window is-right)
        (run-with-idle-timer 0 nil 'palaver-enforce-right-drawer-width))
      window)))

(defun palaver-display-in-appropriate-drawer (buffer alist)
  "Display BUFFER in the appropriate drawer based on rules and frame width.
ALIST is passed to 'display-buffer-in-side-window' internally."
  (let ((side (palaver-buffer-drawer-side buffer)))
    (palaver-display-in-side-window buffer alist side)))

;;; --------------------------------------------------------
;;; Drawer Management Functions
;;; --------------------------------------------------------

(defun palaver-close-drawer (win)
  "Delete drawer window WIN in an appropriate manner."
  (when (window-live-p win)
    (cond
     ((window-parent win)
      ;; Handle atomic windows properly
      (when (window-parameter win 'window-atom)
        (set-window-parameter win 'window-atom nil))
      ;; Use appropriate delete method based on window type
      (if (window-parameter win 'window-side)
          (delete-window win)
        (quit-window nil win)))
     ((frame-parent) (delete-frame))
     (t (quit-window nil win)))))

(defun palaver-update-drawers ()
  "Update drawer state after window configuration change."
  ;; Skip if we're in a minibuffer or if palaver mode is not active
  (unless (or (minibuffer-window-active-p (selected-window))
              (not palaver-mode)
              (frame-parent))
    ;; Check if drawers are open and update state
    (setq palaver-bottom-drawer-open (palaver-get-side-window 'bottom))
    (setq palaver-right-drawer-open (palaver-get-side-window 'right))

    ;; Record current buffers in drawers
    (when palaver-bottom-drawer-open
      (let ((buf (window-buffer palaver-bottom-drawer-open)))
        (palaver-add-drawer-buffer 'bottom buf)))

    (when palaver-right-drawer-open
      (let ((buf (window-buffer palaver-right-drawer-open)))
        (palaver-add-drawer-buffer 'right buf)))

    ;; Update mode lines for all open drawers
    (dolist (side '(bottom right))
      (when-let ((win (palaver-get-side-window side)))
        (with-current-buffer (window-buffer win)
          (with-silent-modifications
            (setq mode-line-format (palaver-modified-mode-line))))))

    ;; Enforce drawer appearance
    (palaver-enforce-right-drawer-width)

    ;; Enforce main window width for active window
    (when (palaver-is-main-window-p (selected-window))
      (palaver-enforce-main-window-width (selected-window)))

    ;; Enforce side-by-side layout
    (palaver-enforce-side-by-side-layout)

    ;; Manage line truncation in all main windows
    (palaver-manage-line-truncation)))

(defun palaver-find-eshell-in-drawer (side)
  "Find an eshell buffer in the SIDE drawer."
  (seq-find (lambda (buf)
              (and (buffer-live-p buf)
                   (with-current-buffer buf
                     (derived-mode-p 'eshell-mode))))
            (palaver-get-drawer-buffers side)))

(defun palaver-show-drawer (side &optional buffer)
  "Show drawer on SIDE, optionally with BUFFER."
  ;; Remember last active main window ONLY if we're actually in a main window
  (when (palaver-is-main-window-p (selected-window))
    (setq palaver-last-active-main-window (selected-window)))

  ;; Determine which buffer to show in the drawer
  (let ((buf (or buffer
                 ;; Use current buffer if it belongs in this drawer
                 (when (and (palaver-buffer-is-drawer-p (current-buffer))
                            (or (eq (buffer-local-value 'palaver-preferred-drawer-side
                                                        (current-buffer)) side)
                                (eq (palaver-buffer-drawer-side (current-buffer)) side)))
                   (current-buffer))
                 ;; Use most recent drawer buffer if available
                 (when-let ((recent (palaver-get-most-recent-drawer-buffer side)))
                   (and (buffer-live-p recent) recent))
                 ;; For bottom drawer, check if there's an eshell in the right drawer
                 (if (eq side 'bottom)
                     (if (palaver-find-eshell-in-drawer 'right)
                         ;; If eshell exists in right drawer, use scratch instead
                         (get-buffer "*scratch*")
                       ;; Otherwise, create a new eshell
                       (eshell t))
                   ;; For right drawer, use scratch as before
                   (get-buffer "*scratch*")))))

    (when (buffer-live-p buf)
      ;; Mark buffer as drawer buffer
      (with-current-buffer buf
        (setq palaver-drawer-status 'drawer)
        (setq-local palaver-preferred-drawer-side side))

      ;; Display and select the buffer
      (select-window (palaver-display-in-side-window buf nil side))

      ;; Update drawer state
      (setq palaver-bottom-drawer-open (eq side 'bottom))
      (setq palaver-right-drawer-open (eq side 'right))
      (palaver-update-drawers)

      ;; Ensure right drawer has correct width
      (when (eq side 'right)
        (palaver-enforce-right-drawer-width)))))

;;;###autoload
(defun palaver-show-bottom-drawer (&optional buffer)
  "Show the bottom drawer, optionally with BUFFER."
  (interactive)
  (palaver-show-drawer 'bottom buffer))

;;;###autoload
(defun palaver-show-right-drawer (&optional buffer)
  "Show the right drawer, optionally with BUFFER."
  (interactive)
  (palaver-show-drawer 'right buffer))

(defun palaver-hide-side-windows (side)
  "Hide side windows on SIDE."
  (dolist (window (window-list))
    (when (palaver-window-is-side-p window side)
      ;; Record the buffer before closing the window
      (let ((buf (window-buffer window)))
        (when (buffer-live-p buf)
          ;; Save this buffer in the MRU list
          (palaver-add-drawer-buffer side buf))
        (palaver-close-drawer window))))

  ;; Update drawer state
  (if (eq side 'bottom)
      (setq palaver-bottom-drawer-open nil)
    (setq palaver-right-drawer-open nil))

  (palaver-update-drawers)

  (when (window-live-p palaver-last-active-main-window)
    (select-window palaver-last-active-main-window)))

;;;###autoload
(defun palaver-hide-bottom-drawer ()
  "Hide the bottom drawer window."
  (interactive)
  (palaver-hide-side-windows 'bottom))

;;;###autoload
(defun palaver-hide-right-drawer ()
  "Hide the right drawer window."
  (interactive)
  (palaver-hide-side-windows 'right))

;;;###autoload
(defun palaver-toggle-drawer (side)
  "Toggle drawer on SIDE.
If drawer is visible but not selected, select it.
If drawer is selected, close it.
If drawer is not visible, show it with the most recently used buffer."
  (unless palaver-mode (palaver-mode 1))
  (let ((drawer-window (palaver-get-side-window side)))
    (cond
     ;; If we're in the drawer, close it
     ((and drawer-window (eq (selected-window) drawer-window))
      (palaver-add-drawer-buffer side (window-buffer drawer-window))
      (palaver-hide-side-windows side))

     ;; If drawer exists but we're not in it, switch to it
     (drawer-window
      (select-window drawer-window)
      ;; Enforce width when switching to existing right drawer
      (when (eq side 'right)
        (run-with-idle-timer 0 nil 'palaver-enforce-right-drawer-width)))

     ;; Otherwise, show the drawer with the most recent buffer
     (t
      (palaver-show-drawer side)))))

;;;###autoload
(defun palaver-toggle-bottom-drawer ()
  "Toggle the bottom drawer window."
  (interactive)
  (palaver-toggle-drawer 'bottom))

;;;###autoload
(defun palaver-toggle-right-drawer ()
  "Toggle the right drawer window."
  (interactive)
  (palaver-toggle-drawer 'right))

(defun palaver-cleanup-drawer-buffers ()
  "Clean up the drawer buffer tracking when mode is disabled."
  (clrhash palaver-drawer-buffers-by-project))

(add-hook 'palaver-mode-hook
          (lambda ()
            (unless palaver-mode
              (palaver-cleanup-drawer-buffers))))

;;;###autoload
(defun palaver-toggle-drawer-location ()
  "Toggle the current buffer between bottom and right drawers.
When in a drawer window, moves the buffer to the other drawer."
  (interactive)
  (unless palaver-mode (palaver-mode 1))
  (let* ((buffer (current-buffer))
         (current-side (window-parameter (selected-window) 'window-side)))
    ;; Only proceed if we're in a drawer
    (if (memq current-side '(bottom right))
        (let ((target-side (if (eq current-side 'bottom) 'right 'bottom)))
          ;; Record this choice for future displays of this buffer
          (dolist (rule palaver-drawer-rules)
            (when (palaver-match-drawer-rule buffer rule)
              (setf (plist-get (cdr rule) :side) target-side)))

          ;; Set buffer-local variable for preferred drawer side
          (with-current-buffer buffer
            (setq-local palaver-preferred-drawer-side target-side))

          ;; Remove buffer from original drawer's MRU list
          (palaver-remove-from-drawer-buffer current-side buffer)

          ;; Display in other drawer
          (palaver-display-in-side-window buffer nil target-side)

          ;; Remove from current drawer
          (delete-window (selected-window))

          ;; Select the new window
          (select-window (get-buffer-window buffer))
          (palaver-update-drawers))
      (message "Not in a drawer window - nothing to toggle"))))

;;;###autoload
(defun palaver-other-window (&optional arg)
  "Window navigation that maintains the two-window philosophy.
If ARG is provided, behaves like `other-window' with that argument.
Otherwise:
- If there's another main window, switch to it (like regular `other-window')
- If there's only one main window and a drawer is open, switch to that drawer
- If there's only one main window, no drawer is open, but a dired window exists, switch to that dired window
- If there's only one main window and no drawer or dired window is open, split it and switch to the new window
- Never creates more than two main windows total
- If in right drawer with two main windows, close the drawer when switching out"
  (interactive "P")
  (if arg
      ;; With prefix arg, use regular other-window behavior
      (other-window (prefix-numeric-value arg))
    ;; Get all main windows (not drawers)
    (let* ((current (selected-window))
           (is-drawer (window-parameter current 'window-side))
           (is-right-drawer (eq is-drawer 'right))
           (main-windows (palaver-get-main-windows))
           (main-window-count (length main-windows))
           (other-mains (seq-remove (lambda (w) (eq w current)) main-windows))
           (target-window nil)
           (close-right-drawer nil))

      (cond
       ;; In a drawer window
       (is-drawer
        (cond
         ;; Last active main window is still alive
         ((window-live-p palaver-last-active-main-window)
          (setq target-window palaver-last-active-main-window))
         ;; Otherwise, find any main window
         (other-mains
          (setq target-window (car other-mains))))

        ;; If we're in the right drawer and there are two main windows,
        ;; mark to close the drawer after selection
        (when (and is-right-drawer (= main-window-count 2))
          (setq close-right-drawer t)))

       ;; In a main window with no other main windows
       ((null other-mains)
        (cond
         ;; Prefer right drawer if open
         ((palaver-get-side-window 'right)
          (setq target-window (palaver-get-side-window 'right)))
         ;; Otherwise try bottom drawer
         ((palaver-get-side-window 'bottom)
          (setq target-window (palaver-get-side-window 'bottom)))
         ;; Check for a dired window that's not the current window
         (t
          (let ((dired-window (seq-find (lambda (window)
                                          (and (not (eq window current))
                                               (with-current-buffer (window-buffer window)
                                                 (derived-mode-p 'dired-mode))))
                                        (window-list))))
            (if dired-window
                (setq target-window dired-window)
              ;; No dired window, create a side-by-side split
              (let ((new-win (split-window current nil 'right)))
                ;; If we have a default buffer to show, use it
                (with-selected-window new-win
                  (when-let ((buf (seq-find
                                   (lambda (b)
                                     (and (buffer-file-name b)
                                          (not (eq b (window-buffer current)))))
                                   (buffer-list))))
                    (switch-to-buffer buf)))
                (setq target-window new-win)))))))

       ;; One other main window - go to it
       ((= (length other-mains) 1)
        (setq target-window (car other-mains)))

       ;; Multiple other windows - use regular other-window
       (t
        (setq target-window (next-window))))

      ;; Always select the target window if we found one
      (when (window-live-p target-window)
        (select-window target-window)
        ;; Update last active main window if we're in a main window
        (when (palaver-is-main-window-p target-window)
          (setq palaver-last-active-main-window target-window)
          ;; Explicitly enforce the window width
          (palaver-enforce-main-window-width target-window)))

      ;; Close right drawer if needed
      (when close-right-drawer
        (palaver-hide-right-drawer))

      ;; Always enforce the side-by-side layout after navigation
      (palaver-enforce-side-by-side-layout))))

;;; --------------------------------------------------------
;;; Window Event Hooks
;;; --------------------------------------------------------

(defun palaver-window-selected-hook (frame &optional _ignored)
  "Hook for when a window in FRAME is selected."
  ;; We can ignore the frame argument and just use (selected-window) directly
  (let ((window (selected-window)))
    (cond
     ;; When a main window is selected
     ((palaver-is-main-window-p window)
      (setq palaver-last-active-main-window window)
      ;; Enforce window width with a slight delay to ensure it works correctly
      (run-with-idle-timer 0 nil 'palaver-enforce-main-window-width window)
      ;; Manage line truncation in all main windows
      (run-with-idle-timer 0 nil 'palaver-manage-line-truncation))

     ;; When the right drawer is selected
     ((eq (window-parameter window 'window-side) 'right)
      ;; Enforce right drawer width when selected
      (run-with-idle-timer 0 nil 'palaver-enforce-right-drawer-width)))))

(defun palaver-customize-drawer-rules ()
  "Customize drawer rules."
  (interactive)
  (customize-variable 'palaver-drawer-rules))

;;; --------------------------------------------------------
;;; Hook Setup Functions
;;; --------------------------------------------------------

(defun palaver-mode-setup-hooks ()
  "Set up hooks for drawer mode."
  (add-hook 'window-configuration-change-hook #'palaver-update-drawers)
  (add-hook 'window-selection-change-functions #'palaver-window-selected-hook))

(defun palaver-mode-remove-hooks ()
  "Remove hooks for drawer mode."
  (remove-hook 'window-configuration-change-hook #'palaver-update-drawers)
  (remove-hook 'window-selection-change-functions #'palaver-window-selected-hook)
  (advice-remove 'quit-window #'palaver-quit-window-advice)
  (advice-remove 'other-window #'palaver-other-window-width-advice)
  (remove-hook 'kill-buffer-hook #'palaver-clean-killed-drawer-buffer))

;;; --------------------------------------------------------
;;; Integration with Other Packages
;;; --------------------------------------------------------

;; Consult integration
(with-eval-after-load 'consult
  (defun palaver-current-window-side ()
    "Return the side of the current window, or nil if not a side window."
    (window-parameter (selected-window) 'window-side))

  ;; Active drawer buffer source that matches the user's consult style
  (defvar palaver-consult-source-active-drawer-buffer
    `(:name     "Active Drawer Buffer"
                :narrow   (?d . "Drawer")
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :enabled  ,(lambda () (palaver-current-window-side))
                :items    ,(lambda ()
                             (let ((side (palaver-current-window-side)))
                               (mapcar #'consult--buffer-pair
                                       (seq-filter #'buffer-live-p
                                                   (palaver-get-drawer-buffers side))))))
    "Active drawer buffer candidate source for palaver.")

  ;; Other drawer buffer source for the opposite drawer
  (defvar palaver-consult-source-other-drawer-buffer
    `(:name     "Other Drawer Buffer"
                :narrow   (?o . "Other")
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :enabled  ,(lambda () (palaver-current-window-side))
                :items    ,(lambda ()
                             (let* ((side (palaver-current-window-side))
                                    (other-side (if (eq side 'bottom) 'right 'bottom)))
                               (mapcar #'consult--buffer-pair
                                       (seq-filter #'buffer-live-p
                                                   (palaver-get-drawer-buffers other-side))))))
    "Other drawer buffer candidate source for palaver.")

  ;; Modified version of project buffer source to exclude drawer buffers
  (defvar palaver-consult-source-project-main-buffer
    `(:name     "Project Buffer"
                :narrow   (?b . "Buffer")
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :enabled  ,(lambda ()
                             (and (not (palaver-current-window-side))
                                  (consult--project-root)))
                :items    ,(lambda ()
                             (when-let (root (consult--project-root))
                               (consult--buffer-query :sort 'visibility
                                                      :directory root
                                                      :as #'consult--buffer-pair
                                                      :predicate (lambda (buffer)
                                                                   (not (palaver-buffer-is-drawer-p buffer)))))))
    "Project main buffer candidate source for palaver."))
;; Handle displaying non-drawer buffers from a drawer window
(defun palaver-display-in-main-window (buffer alist)
  "Display non-drawer BUFFER in a main window when triggered from a drawer.
ALIST is the association list passed to `display-buffer'.
Prefers reusing the single main window if there is only one."
  (let ((current-side (window-parameter (selected-window) 'window-side))
        (target-window nil))

    ;; Only apply this logic when we're in a drawer window
    (when current-side
      ;; First try: Use the last active main window if it's still alive
      (when (and palaver-last-active-main-window
                 (window-live-p palaver-last-active-main-window))
        (setq target-window palaver-last-active-main-window))

      ;; Second try: Get all main windows and use the only one if there's just one
      (unless target-window
        (let ((main-windows (palaver-get-main-windows)))
          (when (= (length main-windows) 1)
            (setq target-window (car main-windows)))))

      ;; If we found a suitable window, use it
      (when target-window
        (set-window-buffer target-window buffer)
        ;; Make sure we update the last active main window
        (setq palaver-last-active-main-window target-window)
        target-window))))

(setq display-buffer-alist
      `(;; Let dired-mode buffers handle their own display logic
        ;; This is important for packages like dirvish-side
        ((lambda (buffer _)
           (with-current-buffer buffer
             (derived-mode-p 'dired-mode)))
         nil)
        ;; Handle buffers that should be in drawers
        ((lambda (buffer _)
           (palaver-buffer-is-drawer-p buffer))
         (display-buffer-reuse-window palaver-display-in-appropriate-drawer)
         (reusable-frames . visible)
         (inhibit-same-window . t))

        ;; When in a drawer and displaying a non-drawer buffer, use main window
        ((lambda (buffer _)
           (and (window-parameter (selected-window) 'window-side) ; In a drawer
                (not (palaver-buffer-is-drawer-p buffer))))      ; Not a drawer buffer
         (palaver-display-in-main-window display-buffer-use-some-window)
         (reusable-frames . visible))

        ;; All other buffers go to the current or most recently selected window
        (".*"
         (display-buffer-same-window))))

;; Handle switch-to-buffer behavior
(setq switch-to-buffer-obey-display-actions t)

;;; --------------------------------------------------------
;;; Drawer management mode
;;; --------------------------------------------------------

;;;###autoload
(define-minor-mode palaver-mode
  "Toggle Drawer management mode.
When enabled, treat certain buffer windows as drawers that can be
summoned or dismissed with commands.

A drawer is a side window (at bottom or right side of frame) that
contains buffers like help, compilation output, REPLs, etc.
Drawers can be toggled, cycled through, and buffers can move
between drawer and regular window status."
  :global t
  :lighter ""
  :group 'palaver

  (if palaver-mode
      ;; Turning the mode ON
      (progn
        ;; First set up all standard mode hooks and tracking
        (palaver-update-drawers)
        (palaver-mode-setup-hooks)
        (palaver-initialize-window-layout)
        (palaver-manage-line-truncation)

        ;; Use pixel-scroll precision mode in Emacs 29
        (when (fboundp 'pixel-scroll-precision-mode)
          (pixel-scroll-precision-mode 1)))

    ;; Turning the mode OFF
    (palaver-mode-remove-hooks)

    ;; Restore all drawer buffers to normal state
    (maphash (lambda (_project-root drawer-pairs)
               (let ((bottom-buffers (car drawer-pairs))
                     (right-buffers (cdr drawer-pairs)))
                 (dolist (buf (append bottom-buffers right-buffers))
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (with-silent-modifications
                         (setq palaver-drawer-status nil
                               mode-line-format (default-value 'mode-line-format))))))))
             palaver-drawer-buffers-by-project)

    ;; Reset variables
    (setq palaver-bottom-drawer-open nil
          palaver-right-drawer-open nil)
    (clrhash palaver-drawer-buffers-by-project)))

;;; --------------------------------------------------------
;;; Drawer minor mode for buffers in drawer windows
;;; --------------------------------------------------------

(defun palaver-display-vertico-in-drawer (buffer alist)
  "Display vertico BUFFER in the current drawer window.
ALIST is the association list passed to `display-buffer'."
  (when (palaver-current-window-side)
    ;; We're in a drawer window, so display the vertico buffer here
    (set-window-buffer (selected-window) buffer)
    (selected-window)))

;; Add configuration for our drawer buffer switching command
(add-to-list 'vertico-multiform-commands
             '(palaver-drawer-switch-buffer . (buffer
                                               (vertico-buffer-display-action . (palaver-display-vertico-in-drawer)))))

(defun palaver-drawer-switch-buffer ()
  "Switch to another drawer buffer using vertico-buffer in the drawer window.
Shows active drawer buffers first, then buffers from the other drawer."
  (interactive)
  (let ((side (palaver-current-window-side)))
    (unless side
      (user-error "Not in a drawer window"))
    ;; Use both active and other drawer buffer sources
    (let ((consult-buffer-sources (list palaver-consult-source-active-drawer-buffer
                                        palaver-consult-source-other-drawer-buffer)))
      (consult-buffer))))

(defvar palaver-drawer-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-x b" #'palaver-drawer-switch-buffer)
    map)
  "Keymap for `palaver-drawer-mode'.")

(define-minor-mode palaver-drawer-mode
  "Minor mode for buffers displayed in palaver drawer windows.
When enabled, provides drawer-specific keybindings and behavior."
  :lighter " Drawer"
  :keymap palaver-drawer-mode-map
  :group 'palaver)

;; Function to update drawer mode when window configuration changes
(defun palaver-update-drawer-mode ()
  "Update drawer mode for all buffers in drawer windows."
  (dolist (window (window-list))
    (let ((side (window-parameter window 'window-side)))
      (when (and window (window-live-p window))
        (with-current-buffer (window-buffer window)
          (if side
              (palaver-drawer-mode 1)
            (palaver-drawer-mode -1)))))))

;; Hook drawer mode into palaver window management
(defun palaver-drawer-mode-setup ()
  "Set up hooks for drawer mode."
  (add-hook 'window-configuration-change-hook #'palaver-update-drawer-mode)
  ;; Ensure drawer mode is set up when palaver displays buffers in drawer windows
  (advice-add 'palaver-display-in-side-window :after
              #'palaver-enable-drawer-mode-after-display))

(defun palaver-enable-drawer-mode-after-display (buffer _alist side)
  "Enable drawer mode after displaying BUFFER in drawer window on SIDE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (palaver-drawer-mode 1))))

(defun palaver-drawer-mode-teardown ()
  "Remove hooks for drawer mode."
  (remove-hook 'window-configuration-change-hook #'palaver-update-drawer-mode)
  (advice-remove 'palaver-display-in-side-window #'palaver-enable-drawer-mode-after-display))

;; Add the drawer mode setup/teardown to palaver-mode
(add-hook 'palaver-mode-hook
          (lambda ()
            (if palaver-mode
                (palaver-drawer-mode-setup)
              (palaver-drawer-mode-teardown))))

;; Initialize if palaver is already active
(when palaver-mode
  (palaver-drawer-mode-setup)
  (palaver-update-drawer-mode))

;; Ensure vertico-multiform-mode is active
(vertico-multiform-mode 1)

;;; --------------------------------------------------------
;;; Setup Functions
;;; --------------------------------------------------------

(defun palaver-initialize-window-layout ()
  "Initialize the window layout.
Creates a clean single-window layout as the starting point."
  (interactive)
  (delete-other-windows)
  (setq palaver-last-active-main-window (selected-window)))

;; Help buffers auto-direct to right drawer
(with-eval-after-load 'help-mode
  (add-hook 'help-mode-hook
            (lambda ()
              (when palaver-mode
                (let ((win (get-buffer-window (current-buffer))))
                  (when (and win (palaver-is-main-window-p win))
                    (palaver-show-right-drawer (current-buffer))))))))

(provide 'palaver)
;;; palaver.el ends here
