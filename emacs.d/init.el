(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(auto-complete color-theme-solarized color-theme deft flx-ido flx framemove fuzzy git-gutter helm-R ess helm-ack helm-ag helm-c-yasnippet helm-projectile helm hexrgb ido-hacks markdown-mode melpa mew minimap popup projectile pkg-info dash s sr-speedbar starter-kit-bindings starter-kit-eshell starter-kit-js starter-kit-lisp elisp-slime-nav starter-kit-ruby starter-kit magit mustache-mode git-rebase-mode git-commit-mode cl-lib ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit inf-ruby sunrise-x-checkpoints sunrise-commander unbound w3m yasnippet coffee-mode noflet smartparens browse-kill-ring shell-pop rainbow-mode ace-jump-mode ace-jump-buffer)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))

(defun package-update-load-path ()
  "Update the load path for newly installed packages."
  (interactive)
  (let ((package-dir (expand-file-name package-user-dir)))
    (mapc (lambda (pkg)
            (let ((stem (symbol-name (car pkg)))
                  (version "")
                  (first t)
                  path)
              (mapc (lambda (num)
                      (if first
                          (setq first nil)
                          (setq version (format "%s." version)))
                      (setq version (format "%s%s" version num)))
                    (aref (cdr pkg) 0))
              (setq path (format "%s/%s-%s" package-dir stem version))
              (add-to-list 'load-path path)))
          package-alist)))
(put 'set-goal-column 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-vcard-avatars-publish nil)
 '(jabber-vcard-avatars-retrieve nil)
 '(minimap-window-location (quote right))
 '(multi-term-dedicated-select-after-open-p t)
 '(multi-term-program "/usr/local/bin/zsh")
 '(multi-term-program-switches "--login"))
