;;; init.el --- Package and configuration initialization.
;;; Commentary:
;; TL;DR: Add packages to my-packages below. Create a $USER.el file
;; with your specific configurations.
;;
;; This file loads the ELPA package archives and installs missing
;; packages. It then sets up load paths and then loads system and user
;; specific configurations. See esk-system-config, esk-user-config,
;; and esk-user-dir below.

;;; Code:
;; We have plenty of ram.
(setq gc-cons-threshold 100000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

(require 'package)
(setq package-enable-at-startup nil)

;; Add package repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(defun esk-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; You can keep system- or user-specific customizations here
(defvar esk-system-config (concat user-emacs-directory (system-name) ".el")
  "The system-specific customization file. Ex. ~/.emacs.d/buelldev.el")
(defvar esk-user-config (concat user-emacs-directory user-login-name ".el")
  "The user specific customization file. Ex. ~/.emacs.d/gmbuell.el")
(defvar esk-user-dir (concat user-emacs-directory user-login-name)
  "The user specific customization directory. Ex. ~/.emacs.d/gmbuell/")

;; Load the user and system specific configurations.
(add-to-list 'load-path esk-user-dir)
(esk-eval-after-init
 '(progn
    (when (file-exists-p esk-user-config) (load esk-user-config))
    ;; (when (file-exists-p esk-user-dir)
    ;;   (mapc 'load (directory-files esk-user-dir t "^[^#].*el$")))
    (when (file-exists-p esk-system-config) (load esk-system-config))))

(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(frame-background-mode (quote light))
 '(package-selected-packages
   (quote
    (xterm-color wand imenu-list color-theme-solarized unicode-fonts ereader sql-indent exec-path-from-shell json-mode emmet-mode web-beautify js2-refactor js2-mode js-doc company-tern tern sauron web-mode wgrep volatile-highlights use-package stickyfunc-enhance smex smartscan smartparens smart-mode-line rainbow-delimiters protobuf-mode perspective pcomplete-extension pcmpl-pip pcmpl-git ov ob-http multiple-cursors multifiles multi-term markdown-mode magit jabber iedit ido-ubiquitous hydra highlight-symbol helm-ls-git helm-descbinds gotham-theme golden-ratio go-errcheck go-eldoc git-gutter framemove flycheck-ycmd flx-ido expand-region esh-buf-stack elfeed discover-my-major deft company-ycmd company-statistics company-go coffee-mode clang-format back-button auto-yasnippet anzu ace-window ace-jump-mode))))

(provide 'init)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
