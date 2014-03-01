(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-buffer ace-jump-mode auto-complete auto-compile browse-kill-ring coffee-mode color-theme color-theme-solarized dash deft elisp-slime-nav ess expand-region flx flx-ido flycheck framemove fuzzy git-commit-mode git-gutter git-rebase-mode gitconfig-mode gitignore-mode helm helm-c-yasnippet helm-ls-git hexrgb ido-hacks ido-ubiquitous inf-ruby magit markdown-mode multi-term mustache-mode noflet pkg-info rainbow-delimiters rainbow-mode ruby-mode s smartparens smex unbound w3m yaml-mode yari yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))

(defun esk-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; You can keep system- or user-specific customizations here
(setq esk-system-config (concat user-emacs-directory system-name ".el")
      esk-user-config (concat user-emacs-directory user-login-name ".el")
      esk-user-dir (concat user-emacs-directory user-login-name))
(add-to-list 'load-path esk-user-dir)
(esk-eval-after-init
 '(progn
    (when (file-exists-p esk-system-config) (load esk-system-config))
    (when (file-exists-p esk-user-config) (load esk-user-config))
    (when (file-exists-p esk-user-dir)
      (mapc 'load (directory-files esk-user-dir t "^[^#].*el$")))))
