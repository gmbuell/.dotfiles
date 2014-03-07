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
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun ensure-package-installed (packages)
  "Assure every package in PACKAGES is installed."
  (when (require 'dash nil :noerror)
    ;; Refresh the package contents before installing new packages
    (when (-difference packages package-activated-list)
      (package-refresh-contents)))
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defvar my-packages '(ace-jump-buffer ace-jump-mode anzu auto-complete browse-kill-ring coffee-mode color-theme color-theme-solarized company dash dedicated deft elisp-slime-nav ess expand-region flx flx-ido flycheck flycheck-google-cpplint framemove fuzzy git-commit-mode git-gutter git-rebase-mode gitconfig-mode gitignore-mode google-c-style helm helm-c-yasnippet helm-ls-git hexrgb ido-hacks ido-ubiquitous iedit inf-ruby jabber jump magit markdown-mode multi-term multiple-cursors mustache-mode noflet pkg-info rainbow-delimiters rainbow-mode ruby-mode s smartparens smex unbound volatile-highlights w3m yaml-mode yari yasnippet)
  "A list of packages to ensure are installed at launch.")

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
    (when (file-exists-p esk-system-config) (load esk-system-config))
    (when (file-exists-p esk-user-config) (load esk-user-config))
    (when (file-exists-p esk-user-dir)
      (mapc 'load (directory-files esk-user-dir t "^[^#].*el$")))))

(provide 'init)

;;; init.el ends here
