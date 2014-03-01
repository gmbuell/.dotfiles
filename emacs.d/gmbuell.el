;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))
;; can't do it at launch or emacsclient won't always honor it
(defun esk-turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)
(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      use-dialog-box nil
      mouse-yank-at-point t
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Ubiquitous Packages which should be loaded on startup rather than
;; autoloaded on demand since they are likely to be used in every
;; session.
(require 'cl)
(require 'ansi-color)

;; Auto-compile emacs lisp files.
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ido
(require 'recentf)
(recentf-mode t)
(ido-mode t)
(require 'ido-hacks)
(ido-hacks-mode)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-use-faces nil  ;; Disabled to see flx highlights
      )
(require 'flx-ido)
(flx-ido-mode t)
(setq gc-cons-threshold 20000000)  ;; For flx

;; Ignore some buffers
(add-to-list 'recentf-exclude "/COMMIT_EDITMSG$")
(add-to-list 'recentf-exclude "/.ido.last$")

(defun ido-clear-history ()
  "Clear ido virtual buffers list."
  (interactive)
  (setq ido-virtual-buffers '())
  (setq recentf-list '()))
(setq ido-ignore-buffers
      (append ido-ignore-buffers '("^\*Helm")))

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; Plain text settings
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Try and use aspell but fall back to ispell
'(if (executable-find "aspell")
     '(progn
        (setq ispell-program-name "aspell")
        (setq ispell-list-command "list")
        (require 'ispell)
        (require 'flyspell)
   (when (executable-find "ispell")
     (setq ispell-program-name "aspell")
     (setq ispell-list-command "list")
     (require 'ispell)
     (require 'flyspell))))

(eval-after-load 'ispell
  '(progn
     (add-hook 'text-mode-hook 'turn-on-flyspell)
     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
     (ac-flyspell-workaround)
     (setq flyspell-issue-message-flag nil
           flyspell-issue-welcome-flag nil)))

(random t)  ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))

     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Better Defaults
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)

(require 'helm-config)
(require 'helm-ls-git)
;;(global-set-key (kbd "C-c h") 'helm-browse-project)
(global-set-key (kbd "C-c h") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c C-o") 'helm-occur)
(global-set-key (kbd "C-c M-o") 'helm-multi-occur)
(global-set-key (kbd "C-c c-b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Consider giving helm-adaptative-mode a try

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(load-theme 'solarized-dark t)
;; Nice fonts:
;; Inconsolata-11
;; Droid Sans Mono-11
;; DejaVu Sans Mono-11
;; Note, these require: apt-get install ttf-droid ttfinconsolata
;; ttf-dejavu
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)

(require 'auto-complete-autoloads)
(require 'auto-complete-config)
(ac-config-default)

(require 'ess-site)

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(require 'deft)
;;(setq deft-text-mode 'gfm-mode)

;; Turn on red highlighting for characters outside of the 80/100 char limit
(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'ess-mode (font-lock-width-keyword 80))

(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun fontify-hex-colors (limit)
  (remove-overlays (point) limit 'fontify-hex-colors t)
  (while (re-search-forward "#[[:xdigit:]]\\{6\\}" limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0))))
      (overlay-put ov 'face
                   (list :background (match-string-no-properties 0)
                         :foreground
                         (if (> 128.0 (hexcolour-luminance (match-string-no-properties 0)))
                             "#fdf6e3" "#002b36")))
      (overlay-put ov 'fontify-hex-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this function
  nil)

(font-lock-add-keywords 'lisp-interaction-mode '((fontify-hex-colors)))
(font-lock-add-keywords 'emacs-lisp-mode '((fontify-hex-colors)))
(font-lock-add-keywords 'ess-mode '((fontify-hex-colors)))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;; Code health
(column-number-mode t)
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'git-gutter)
(global-git-gutter-mode t)

;; w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(eval-after-load 'w3m
  '(setq w3m-use-cookies t))

(require 'magit)
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#5f8700")
     (set-face-foreground 'diff-removed "#dc322f")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "#5f8700")
     (set-face-foreground 'magit-diff-del "#dc322f")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; From http://www.emacswiki.org/emacs/OccurMode
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (with-current-buffer "*Occur*"
        (goto-char (point-min))
        (read-only-mode)
        (if (looking-at "^[0-9]+ lines matching \"")
            (kill-line 1))
        (while (re-search-forward "^[ \t]*[0-9]+:"
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1)))
    (message "There is no buffer named \"*Occur*\".")))
(define-key occur-mode-map (kbd "C-c C-x") 'occur-mode-clean-buffer)

(global-auto-revert-mode)
(setq auto-revert-check-vc-info t)

;; Enhancements to dired including dired-jump
(require 'dired-x)

(require 'jabber-autoloads)
(eval-after-load 'jabber
  '(setq jabber-account-list
         '(("gmbuell@gmail.com"
            (:network-server . "talk.google.com")
            (:connection-type . ssl)))
         ;; Disable jabber images
         jabber-chat-buffer-show-avatar nil
         jabber-vcard-avatars-publish nil
         jabber-vcard-avatars-retrieve nil
         jabber-history-enabled t
         jabber-use-global-history nil
         ;; Don't show presense notifications.
         jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil)
         ;; Don't show alerts if I'm already in the chat buffer.
         jabber-message-alert-same-buffer nil))

(require 'coffee-mode)
(eval-after-load 'coffee-mode
  '(progn
     (setq coffee-tab-width 2)
     (define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-file)))

(require 'smartparens-config)
;;(smartparens-global-mode t)
(global-flycheck-mode)

;;(require 'browse-kill-ring)
;;(browse-kill-ring-default-keybindings)
;; Using helm for this instead

(global-unset-key (kbd "<f1>"))

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-p") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c C-n") 'ace-jump-line-mode)

(require 'rainbow-delimiters)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)
(add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(eval-after-load "dash" '(dash-enable-font-lock))

(require 'multi-term)
(define-key global-map (kbd "<f1>") 'multi-term-dedicated-toggle)
(eval-after-load 'multi-term
  '(setq
    multi-term-dedicated-select-after-open-p t
     ;; Use zsh for multi-term
    multi-term-program "/usr/local/bin/zsh"
    multi-term-program-switches "--login"))

(server-start)
