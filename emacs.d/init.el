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
(require 'package)
(setq package-enable-at-startup nil)

;; Add package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Get dash so we can install everything else that hasn't been
;; migrated to use-package.
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))

(defun ensure-package-installed (packages)
  "Assure every package in PACKAGES is installed."
  (when (require 'dash nil :noerror)
    ;; Refresh the package contents before installing new packages
    (when (-difference packages package-activated-list)
      (progn
        (package-refresh-contents)
        (dolist (p packages)
          (when (not (package-installed-p p))
            (package-install p)))))
    ))

(defvar my-packages '(ace-jump-buffer ace-jump-mode ace-window anzu auto-complete back-button base16-theme browse-kill-ring coffee-mode color-theme color-theme-solarized company company-go dash dedicated deft discover-my-major elfeed elisp-slime-nav ess expand-region flx flx-ido flycheck flycheck-google-cpplint framemove fuzzy git-commit-mode git-gutter git-rebase-mode git-timemachine gitconfig-mode gitignore-mode go-eldoc go-errcheck go-mode google-c-style helm helm-c-yasnippet helm-ls-git helm-swoop hexrgb highlight-symbol ido-hacks ido-ubiquitous iedit inf-ruby jabber jump magit markdown-mode multi-term multiple-cursors mustache-mode noflet pkg-info rainbow-delimiters readline-complete ruby-mode s smart-mode-line smartparens smex unbound volatile-highlights yaml-mode yari yasnippet ycmd company-ycmd gotham-theme dash-functional smart-forward hydra multifiles flycheck-ycmd company-c-headers sr-speedbar)
  "A list of packages to ensure are in
stalled at launch.")

(ensure-package-installed my-packages)

(defun esk-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; You can keep system- or user-specific customizations here
(defvar esk-system-config (concat user-emacs-directory system-name ".el")
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
    (when (file-exists-p esk-user-dir)
      (mapc 'load (directory-files esk-user-dir t "^[^#].*el$")))
    (when (file-exists-p esk-system-config) (load esk-system-config))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" default))))

(put 'narrow-to-region 'disabled nil)
