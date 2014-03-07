;;; gmbuell --- User specific configuration.
;;; Commentary:
;; Requires the setup performed in init.el.
;;
;; Useful key bindings defined here (in order of importance):
;; -------------------------------------------------------------------
;; "C-s" -> isearch-forward-regexp
;; Incremental forward search (by regexp)

;; "C-r" -> isearch-backward-regexp
;; Incremental backward search (by regexp)

;; "C-c h" -> helm-ls-git-ls
;; Open files that share the same git project as the current file. See
;; section on helm below for pro tips.

;; "M-y" -> helm-show-kill-ring
;; Search through the kill ring. This replaces the standard interface
;; for searching backwards through the kill ring with repeated
;; invocations of "M-y".

;; "C-c g" -> magit-status
;; Opens the git interface for the current project.

;; "<f1>" -> multi-term-dedicated-toggle
;; Shows a shell. Hit "<f1>" again to hide the shell.

;; "C-x C-b" -> helm-buffers-list
;; List buffers using helm. For when you need something more powerful
;; than the standard ido smart buffer switching with "C-x b"

;; "C-c C-o" -> helm-occur
;; Search for something in a buffer and see all the results
;; simultaneously.

;; "C-c M-o" -> helm-multi-occur
;; Search for something in multiple buffers and see all the results
;; simultaneously.

;; "C-c c-b" -> helm-bookmarks
;; Search through bookmarks.

;;; Code:
;; This first section cleans up the Emacs window and interface.

;; Turn off mouse interface early in startup to avoid momentary display.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))
;; can't do it at launch or Emacsclient won't always honor it
(defun esk-turn-off-tool-bar ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
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

;; Better Defaults
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'saveplace)
(setq-default save-place t)

;; Replace all the search keys with regex versions.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Activate occur easily inside isearch
;; Note: this might already be enabled in emacs 24
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

;; We need nested minibuffers mostly due to helm replacing "M-y"
(setq enable-recursive-minibuffers t)

;; Ubiquitous Packages which should be loaded on startup rather than
;; autoloaded on demand since they are likely to be used in every
;; session.
(require 'cl)
(require 'ansi-color)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Ido configuration. Gives smart buffer switching and menus.
;; -------------------------------------------------------------------
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

;; Ignore some buffers.
(add-to-list 'recentf-exclude "/COMMIT_EDITMSG$")
(add-to-list 'recentf-exclude "/.ido.last$")

;; Ido uses "virtual buffers" so that you do not have to re-open files
;; between emacs sessions. This can get out of hand sometimes and this
;; command will clean everything up.
(defun ido-clear-history ()
  "Clear ido virtual buffers list."
  (interactive)
  (setq ido-virtual-buffers '())
  (setq recentf-list '()))
(setq ido-ignore-buffers
      (append ido-ignore-buffers '("^\*Helm")))

;; Smex is for smart completion of M-x commands.
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Find File at Point. Makes sevaral commands smarter when the cursor
;; is on something relevant.
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

;; Command to open files in another window. Useful for split buffer
;; setups.
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(set-default 'imenu-auto-rescan t)

;; Hippie expand: at times perhaps too hip.
(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))
     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))
(global-set-key (kbd "M-/") 'hippie-expand)

;; Helm configuration. Helm is a great interface for finding things.
;; Protip: Helm uses space as a separator. So if you use helm to
;; switch to a buffer titled 'gmbuell-ruby.el', you could narrow using
;; "gm rub" and the space separator would start the other match
;; section. In regex speak, this is like space being translated to .*
;; This is unlike ido mode which transparently inserts virtual '.'
;; characters for matches.
(require 'helm-config)
(require 'helm-ls-git)
;; This is my main interface to opening/switching between files in the
;; same git repository. It is particularly useful because the virtual
;; buffers of ido mode can make finding a project specific file
;; difficult.
(global-set-key (kbd "C-c h") 'helm-ls-git-ls)
;; helm-ls-git-ls is specific to git projects. helm-browse-project
;; might be more general. Ex.
;; (global-set-key (kbd "C-c h") 'helm-browse-project)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c C-o") 'helm-occur)
(global-set-key (kbd "C-c M-o") 'helm-multi-occur)
(global-set-key (kbd "C-c c-b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'helm-imenu)
;; Consider giving helm-adaptative-mode a try

;; Emacs theme.
;; http://ethanschoonover.com/solarized
(load-theme 'solarized-dark t)
;; Nice fonts:
;; Inconsolata-11
;; Droid Sans Mono-11
;; DejaVu Sans Mono-11
;; Note, these require: apt-get install ttf-droid ttfinconsolata
;; ttf-dejavu
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

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
     ;;(ac-flyspell-workaround) not needed since company mode is now used.
     (setq flyspell-issue-message-flag nil
           flyspell-issue-welcome-flag nil)))

;; Emacs expects sentences to end with double spaces. This is crazy.
(setq sentence-end-double-space nil)

;; Code health
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
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

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Flycheck for linting
(global-flycheck-mode)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(require 'flycheck-google-cpplint)
;; Add Google C++ Style checker.
;; In default, syntax checked by Clang and Cppcheck.
(flycheck-add-next-checker 'c/c++-cppcheck
                           '(warnings-only . c/c++-googlelint))

;; Company mode for completions: http://company-mode.github.io/
(require 'company)
(add-to-list 'company-backends 'company-capf)
(setq company-global-modes '(c-mode c++-mode emacs-lisp-mode))
(global-company-mode)

;; auto-complete has been disabled in favor of company mode.
;;(require 'auto-complete-autoloads)
;;(require 'auto-complete-config)
;;(ac-config-default)

;; Git integration
;; -------------------------------------------------------------------
;; Shows git diff information in the gutter.
(require 'git-gutter)
(global-git-gutter-mode t)

;; Magit makes git inside emacs awesome.
;; Start with "C-c g"
;; http://daemianmack.com/magit-cheatsheet.html
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
(global-set-key (kbd "C-c g") 'magit-status)

(global-auto-revert-mode)
(setq auto-revert-check-vc-info t)
(setq vc-follow-symlinks t)

(require 'multi-term)
(define-key global-map (kbd "<f1>") 'multi-term-dedicated-toggle)
(eval-after-load 'multi-term
  '(progn
     (setq
      multi-term-dedicated-select-after-open-p t
      ;; Use zsh for multi-term
      multi-term-program "/usr/local/bin/zsh"
      multi-term-program-switches "--login"
      ;; Don't pass C-y through to the shell.
      term-unbind-key-list (-difference term-unbind-key-list '("C-y" "C-l")))
     (add-to-list 'term-bind-key-alist '("C-l" . recenter-top-bottom))
     (add-to-list 'term-bind-key-alist '("C-f" . forward-char))
     (add-to-list 'term-bind-key-alist '("C-b" . backward-char))))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)

;; Sets up Shift + arrow keys for moving between frames and windows.
;; This is particularly useful for multi-monitor setups.
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Deft is my preferred note-taking setup. See:
;; http://jblevins.org/projects/deft/

(require 'deft)
(eval-after-load 'deft
  '(progn
     ;; Set the deft directory to Dropbox/notes if it exists.
     (when (file-exists-p "~/Dropbox")
       (setq deft-directory "~/Dropbox/notes")
       (when (not (file-exists-p deft-directory))
         (make-directory deft-directory t)))
     ;;(setq deft-text-mode 'gfm-mode)  ;; Use Github flavored Markdown
     ;;(setq deft-use-filename-as-title t)
     ))

;; w3m (web browsing inside Emacs)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(eval-after-load 'w3m
  '(setq w3m-use-cookies t))

;; Google Talk inside Emacs.
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

;; Settings and modes to make text entry and display smarter.
;; -------------------------------------------------------------------
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

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(require 'smartparens-config)
;;(smartparens-global-mode t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Give visual feedback on some commands
;;(require 'volatile-highlights)
;;(volatile-highlights-mode t)
;; Set highlight faces for solarized.
;; (set-face-attribute 'vhl/default-face nil
;;                     :background "#586e75"
;;                     :foreground "#93a1a1")
;; Disable modeline for volatile highlights
;;(setcar (cdr (assq 'volatile-highlights-mode minor-mode-alist)) nil)

;; Show search progress
(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "#b58900" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter nil)
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

(require 'rainbow-delimiters)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)
(add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)

;; Not currently used.
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key global-map (kbd "C-c C-p") 'ace-jump-line-mode)
;; (define-key global-map (kbd "C-c C-n") 'ace-jump-line-mode)

;; Miscellaneous modes
;; -------------------------------------------------------------------

;; Enhancements to dired including dired-jump
(require 'dired-x)

(eval-after-load "dash" '(dash-enable-font-lock))
(require 'coffee-mode)
(eval-after-load 'coffee-mode
  '(progn
     (setq coffee-tab-width 2)
     (define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-file)))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; Miscellaneous utility functions
;; -------------------------------------------------------------------

;; From http://www.emacswiki.org/emacs/OccurMode
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (with-current-buffer "*Occur*"
        (goto-char (point-min))
        (setq buffer-read-only nil)
        (if (looking-at "^[0-9]+ lines matching \"")
            (kill-line 1))
        (while (re-search-forward "^[ \t]*[0-9]+:"
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1)))
    (message "There is no buffer named \"*Occur*\".")))
(define-key occur-mode-map (kbd "C-c C-x") 'occur-mode-clean-buffer)

;; Note, this function is not currently bound to a key.
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(server-start)

(provide 'gmbuell)

;;; gmbuell.el ends here
