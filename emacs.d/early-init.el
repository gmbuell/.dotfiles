(setq package-quickstart t)

;; In batch mode, Emacs skips the normal startup sequence that calls
;; package-activate-all between early-init and init. Do it ourselves.
(when noninteractive
  (require 'package)
  (package-activate-all))

;; Maximize GC threshold during init, restore after startup
(setq gc-cons-threshold most-positive-fixnum)

;; Skip regex matching on file loads during init
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 100000000)
    (setq file-name-handler-alist default-file-name-handler-alist)))

;; Auto-recompile init.el when stale (runs after startup, benefits next launch)
(defun gb--recompile-init ()
  "Recompile init.el and refresh package-quickstart if stale."
  (let* ((init-el (locate-user-emacs-file "init.el"))
         (init-elc (byte-compile-dest-file init-el)))
    (when (file-newer-than-file-p init-el init-elc)
      (byte-compile-file init-el)
      (when (native-comp-available-p)
        (native-compile-async init-el))))
  (let* ((qs (locate-user-emacs-file "package-quickstart.el"))
         (qsc (locate-user-emacs-file "package-quickstart.elc")))
    (when (and (file-exists-p qs)  ;; install.sh handles initial generation
               (cl-some (lambda (dir)
                          (file-newer-than-file-p dir qs))
                        (directory-files
                         (expand-file-name "elpa" user-emacs-directory)
                         t "\\`[^.]")))
      (require 'mosey) ;; mosey autoloads call defmosey macro at top-level
      (package-quickstart-refresh))
    ;; Ensure .elc is up to date (symlink can cause .el and .elc to diverge)
    (when (file-newer-than-file-p qs qsc)
      (byte-compile-file qs))))
(add-hook 'emacs-startup-hook #'gb--recompile-init)

;; Disable UI chrome before frames are drawn
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) initial-frame-alist)
(push '(tool-bar-lines . 0) initial-frame-alist)
(push '(vertical-scroll-bars) initial-frame-alist)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
