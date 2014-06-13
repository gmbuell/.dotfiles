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
      (progn
        (package-refresh-contents)
        (dolist (p packages)
          (when (not (package-installed-p p))
            (package-install p)))))
    ))

(defvar my-packages '(ace-jump-buffer ace-jump-mode anzu auto-complete back-button base16-theme browse-kill-ring coffee-mode color-theme color-theme-solarized company company-go dash dedicated deft discover-my-major elfeed elisp-slime-nav ess expand-region flx flx-ido flycheck flycheck-google-cpplint framemove fuzzy git-commit-mode git-gutter git-rebase-mode gitconfig-mode gitignore-mode go-eldoc go-errcheck go-mode google-c-style helm helm-c-yasnippet helm-ls-git helm-swoop hexrgb highlight-symbol ido-hacks ido-ubiquitous iedit inf-ruby jabber jump magit markdown-mode multi-term multiple-cursors mustache-mode noflet pkg-info rainbow-delimiters readline-complete ruby-mode s smart-mode-line smartparens smex unbound volatile-highlights yaml-mode yari yasnippet)
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
 '(flycheck-clang-include-path (quote ("/google/src/head/depot/google3")))
 '(flycheck-completion-system (quote ido))
 '(sml/active-background-color "#505050")
 '(sml/active-foreground-color "#e0e0e0")
 '(sml/inactive-background-color "#202020")
 '(sml/inactive-foreground-color "#e0e0e0")
 '(sp-override-key-bindings
   (quote
    (("C-<right>" . sp-slurp-hybrid-sexp)
     ("C-<left>" . sp-dedent-adjust-sexp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/charging ((t (:inherit sml/global :foreground "#90a959"))))
 '(sml/client ((t (:inherit sml/prefix :foreground "#d28445"))))
 '(sml/discharging ((t (:foreground "#ac4142" :inherit sml/global))))
 '(sml/filename ((t (:weight bold :foreground "#f4bf75" :inherit sml/global))))
 '(sml/global ((t (:inverse-video nil :foreground "#b0b0b0"))))
 '(sml/modes ((t (:foreground "#e0e0e0" :inherit sml/global))))
 '(sml/modified ((t (:weight bold :foreground "#ac4142" :inherit sml/global))))
 '(sml/outside-modified ((t (:background "#ac4142" :foreground "#e0e0e0" :inherit sml/global))))
 '(sml/prefix ((t (:foreground "#aa759f" :inherit sml/global))))
 '(sml/process ((t (:inherit sml/prefix :foreground "#d28445"))))
 '(sml/read-only ((t (:foreground "#6a9fb5" :inherit sml/global))))
 '(sml/vc-edited ((t (:inherit sml/prefix :foreground "#d28445"))))
 '(which-func ((t (:foreground "#75b5aa")))))
