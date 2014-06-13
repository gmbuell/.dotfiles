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
(setq save-place-file "~/.emacs.d/saved-places")

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

;; From http://steckerhalter.co.vu/steckemacs.html
;; Make isearch-forward put the cursor at the start of the search, not the end.
(defun my-isearch-goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-isearch-goto-match-beginning)

;; Delete more than one space
(setq-default c-hungry-delete-key t)

(setq highlight-symbol-on-navigation-p t)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

(require 'back-button)
(back-button-mode 1)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i")
;; 'helm-swoop-from-evil-search)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

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
      ido-enable-flex-matching nil
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-use-faces nil  ;; Disabled to see flx highlights
      )
(require 'flx-ido)
(flx-ido-mode t)
;; (setq gc-cons-threshold 20000000)  ;; For flx

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
(global-set-key (kbd "C-c C-b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; Jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'helm-imenu)
;; Consider giving helm-adaptative-mode a try

;; Emacs theme.
(load-theme 'base16-default t)
;; Nice fonts:
;; Inconsolata-11
;; Droid Sans Mono-11
;; DejaVu Sans Mono-11
;; Note, these require: apt-get install ttf-droid ttfinconsolata
;; ttf-dejavu
(set-frame-font "DejaVu Sans Mono 11" t t)

;; Plain text settings
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Try and use aspell but fall back to ispell
'(if (executable-find "aspell")
     (progn
        (setq ispell-program-name "aspell")
        (setq ispell-list-command "list")
        (require 'ispell)
        (require 'flyspell)
   (when (executable-find "ispell")
     (setq ispell-program-name "ispell")
     (require 'ispell)
     (require 'flyspell))))

(eval-after-load 'ispell
  '(progn
     (add-hook 'text-mode-hook 'turn-on-flyspell)
     (add-hook 'markdown-mode-hook 'turn-on-flyspell)
     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
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
(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
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
(font-lock-add-keywords 'markdown-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'go-mode (font-lock-width-keyword 80))

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
;;(require 'google-c-style)
;;(add-hook 'c-mode-common-hook 'google-set-c-style)
;;(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;;(require 'flycheck-google-cpplint)
;; Add Google C++ Style checker.
;; In default, syntax checked by Clang and Cppcheck.
;; (flycheck-add-next-checker 'c/c++-cppcheck
;;                            '(warnings-only . c/c++-googlelint))

;; Company mode for completions: http://company-mode.github.io/
(require 'company)
(add-to-list 'company-backends 'company-capf)
;;(setq company-global-modes '(c-mode c++-mode emacs-lisp-mode))
(global-company-mode)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
;;(add-to-list 'company-backends 'company-files t)

;; Git integration
;; -------------------------------------------------------------------
;; Shows git diff information in the gutter.
(require 'git-gutter)
(global-git-gutter-mode t)

;; Magit makes git inside emacs awesome.
;; Start with "C-c g"
;; http://daemianmack.com/magit-cheatsheet.html
(require 'magit)
;; (eval-after-load 'diff-mode
;;   '(progn
;;      (set-face-foreground 'diff-added "#5f8700")
;;      (set-face-foreground 'diff-removed "#dc322f")))
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "#5f8700")
;;      (set-face-foreground 'magit-diff-del "#dc322f")
;;      (when (not window-system)
;;        (set-face-background 'magit-item-highlight "black"))))
(global-set-key (kbd "C-c g") 'magit-status)

(global-auto-revert-mode)
(setq auto-revert-check-vc-info t)
(setq vc-follow-symlinks t)

(require 'multi-term)
(define-key global-map (kbd "<f1>") 'multi-term-dedicated-toggle)
(define-key global-map (kbd "C-c s") 'multi-term-dedicated-toggle)
(eval-after-load 'multi-term
  '(progn
     (setq
      multi-term-dedicated-select-after-open-p t
      ;; Use zsh for multi-term
      multi-term-program "/usr/local/bin/zsh"
      multi-term-program-switches "--login"
      ;; Don't pass C-y through to the shell.
      ;;term-unbind-key-list (-difference term-unbind-key-list '("C-y"
      ;;"C-l" "C-w")))
      )
      ;;(add-to-list 'term-bind-key-alist '("C-l" . recenter-top-bottom))
      ;;(add-to-list 'term-bind-key-alist '("C-f" . forward-char))
      ;;(add-to-list 'term-bind-key-alist '("C-b" . backward-char))
      ;;(add-to-list 'term-bind-key-alist '("C-w" . kill-region))
      ))

;; (add-hook 'term-mode-hook (lambda ()
;;                             (define-key term-raw-map (kbd "C-y") 'term-paste)))
;; (add-hook 'term-mode-hook (lambda ()
;;                             (define-key term-raw-map (kbd "C-w") 'kill-region)))
(add-hook 'term-mode-hook
          (lambda ()
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

;; eww browsing inside emacs. Much better than w3m!
(setq browse-url-browser-function 'eww-browse-url)

;; Google Talk inside Emacs.
(require 'jabber)
;;(require 'jabber-autoloads)
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
         jabber-message-alert-same-buffer nil
         jabber-history-dir "~/.jabber"))

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

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(require 'smartparens-config)
(smartparens-global-mode t)
(sp-use-smartparens-bindings)
(custom-set-variables
 '(sp-override-key-bindings
   '(("C-<right>" . sp-slurp-hybrid-sexp)
     ("C-<left>" . sp-dedent-adjust-sexp))))

;; My custom code to make kill line more intelligent
(require 'subr-x)
(defun gmbuell-smart-kill-line (arg)
  "If the line is only whitespace or the command is prefixed with C-u,
   use standard kill-line. Otherwise, use sp-kill-hybrid-sexp"
  (interactive "P")
  (if (or arg (string-blank-p (thing-at-point 'line)))
      (progn (kill-line nil)
             (indent-for-tab-command))
    (progn (sp-kill-hybrid-sexp arg)
           (indent-for-tab-command)))
  )

(defun setup-sp-bindings ()
  (local-set-key (kbd "C-k") 'gmbuell-smart-kill-line))
(add-hook 'prog-mode-hook 'setup-sp-bindings t)

;; Fix forward slurp spacing
;; https://github.com/Fuco1/smartparens/issues/297
(sp-local-pair 'c-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*")
(sp-local-pair 'c++-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*)")
;; https://github.com/Fuco1/smartparens/issues/236
;; (sp-local-pair 'c++-mode
;;                "(" nil
;;                :pre-handlers '(sp-fixspace-pre-slurp-handler))
;; (sp-local-pair 'c-mode
;;                "(" nil
;;                :pre-handlers '(sp-fixspace-pre-slurp-handler))

;; (defun sp-fixspace-pre-slurp-handler (id action context)
;;   (when (eq action 'slurp-forward)
;;     ;; if there was no space before, there shouldn't be after either
;;     ;; ok = enclosing, next-thing one being slurped into
;;     (save-excursion
;;       (when (and (= (sp-get ok :end) (sp-get next-thing :beg))
;;                  (equal (sp-get ok :op) (sp-get next-thing :op)))
;;         (goto-char (sp-get ok :end))
;;         (when (looking-back " ")
;;           (delete-char -1))))))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Give visual feedback on some commands
(require 'volatile-highlights)
(volatile-highlights-mode t)
;; Set highlight faces for solarized.
;; (set-face-attribute 'vhl/default-face nil
;;                     :background "#586e75"
;;                     :foreground "#93a1a1")
;; Disable modeline for volatile highlights
(setcar (cdr (assq 'volatile-highlights-mode minor-mode-alist)) nil)

;; Show search progress
(require 'anzu)
(global-anzu-mode +1)
;; (set-face-attribute 'anzu-mode-line nil
;;                     :foreground "#b58900" :weight 'bold)
(eval-after-load 'anzu
  (setq
   anzu-mode-lighter nil
   anzu-deactivate-region t
   anzu-search-threshold 1000
   anzu-replace-to-string-separator " => "))

(setq
 elfeed-feeds
 (quote
  ("http://www.cardgamedb.com/forums/index.php?/rss/ccs/1c61-Game%20of%20Thrones/"))
 shr-blocked-images ".+")

(require 'rainbow-delimiters)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)
(add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)

;; Miscellaneous modes
;; -------------------------------------------------------------------

;; Enhancements to dired including dired-jump
(require 'dired-x)

(eval-after-load 'dash '(dash-enable-font-lock))
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
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
;; Rebind C-g to completely exit minibuffers, no matter how nested.
(global-set-key (kbd "C-g") 'top-level)

;; For chromebook:
(global-set-key (kbd "<deletechar>") 'backward-kill-word)

;; Register
(defun copy-to-j (start end)
  "Copy the text in the region to register 'j'."
  (interactive "r")
  (copy-to-register ?j start end))
(defun paste-from-j ()
  "Paste the text from register 'j'."
  (interactive)
  (insert-register ?j t))
(define-key global-map (kbd "C-c C-j") 'copy-to-j)
(define-key global-map (kbd "C-j") 'paste-from-j)

;; Add zsh history search to helm
(defvar helm-c-source-zsh-history
  '((name . "Zsh History")
    (candidates . helm-c-zsh-history-set-candidates)
    (action . (("Execute Command" . helm-c-zsh-history-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defun helm-c-zsh-history-set-candidates (&optional request-prefix)
  (let ((pattern (replace-regexp-in-string
                  " " ".*"
                  (or (and request-prefix
                           (concat request-prefix
                                   " " helm-pattern))
                      helm-pattern))))
    (with-current-buffer (find-file-noselect "~/.zsh_history" t t)
      (auto-revert-mode -1)
      (goto-char (point-max))
      (loop for pos = (re-search-backward pattern nil t)
            while pos
            collect (replace-regexp-in-string
                     "\\`:.+?;" ""
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))))))

(defun helm-c-zsh-history-action (candidate)
  (multi-term-dedicated-select)
  (kill-new candidate)
  (yank))

(defun helm-command-from-zsh ()
  (interactive)
  (require 'helm)
  (helm-other-buffer 'helm-c-source-zsh-history "*helm zsh history*"))

(define-key global-map (kbd "C-c C-z") 'helm-command-from-zsh)

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "gfm")

;; which-function-mode
;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
;; (which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line, because it's mostly
;;       ;; invisible here anyway.
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

;; smart-mode-line
;; https://github.com/Bruce-Connor/smart-mode-line
(setq sml/theme 'respectful)
(sml/setup)
(setq sml/mode-width (quote full)
      sml/mule-info nil
      sml/position-percentage-format nil
      sml/show-remote nil
      sml/size-indication-format "")
(add-to-list 'sml/hidden-modes magit-auto-revert-mode-lighter)
(add-to-list 'sml/hidden-modes " GitGutter")

;; base16-theme customizations
(setq background-theme-color "#202020"
      current-line-theme-color "#505050"
      selection-theme-color "#b0b0b0"
      foreground-theme-color "#e0e0e0"
      comment-theme-color "#b0b0b0"
      cursor-theme-color "#e0e0e0"
      red-theme-color "#ac4142"
      orange-theme-color "#d28445"
      yellow-theme-color "#f4bf75"
      green-theme-color "#90a959"
      aqua-theme-color "#75b5aa"
      blue-theme-color "#6a9fb5"
      purple-theme-color "#aa759f"
      )

(custom-set-variables
 `(sml/active-background-color ,current-line-theme-color)
 `(sml/active-foreground-color ,foreground-theme-color)
 `(sml/inactive-background-color ,background-theme-color)
 `(sml/inactive-foreground-color ,foreground-theme-color)
 )
(custom-set-faces
 `(sml/charging ((t (:inherit sml/global :foreground ,green-theme-color))))
 `(sml/discharging ((t (:foreground ,red-theme-color :inherit sml/global))))
 `(sml/filename ((t (:weight bold :foreground ,yellow-theme-color :inherit sml/global))))
 `(sml/global ((t (:inverse-video nil :foreground ,selection-theme-color))))
 `(sml/modes ((t (:foreground ,foreground-theme-color :inherit sml/global))))
 `(sml/modified ((t (:weight bold :foreground ,red-theme-color :inherit sml/global))))
 `(sml/outside-modified
   ((t (:background ,red-theme-color :foreground ,foreground-theme-color :inherit
                    sml/global))))
 `(sml/prefix ((t (:foreground ,purple-theme-color :inherit sml/global))))
 `(sml/read-only ((t (:foreground ,blue-theme-color :inherit sml/global))))
 `(sml/client ((t (:inherit sml/prefix :foreground ,orange-theme-color))))
 `(sml/process ((t (:inherit sml/prefix :foreground ,orange-theme-color))))
 `(sml/vc-edited ((t (:inherit sml/prefix :foreground ,orange-theme-color))))
 `(which-func ((t (:foreground ,aqua-theme-color)))))

;; My pinky hurts. Lets try out ace-jump-mode.
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(ace-jump-mode-enable-mark-sync)
(setq ace-jump-mode-scope 'window)
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; go-mode
;; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;; http://dominik.honnef.co/posts/2013/08/writing_go_in_emacs__cont__/
(require 'go-mode-load)
;; https://github.com/dougm/goflymake
;; go get -u github.com/dougm/goflymake
(add-to-list 'load-path (substitute-in-file-name "$GOPATH/src/github.com/dougm/goflymake"))
(require 'go-flycheck)

;; https://github.com/nsf/gocode/tree/mast~/gocodeer/emacs-company
;; go get -u github.com/nsf/gocode
(require 'company)
(require 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-minimum-prefix-length 3)               ; autocomplete right after '.'
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start
                                        ; autocompletion only after
                                        ; typing
;; Only use company mode for go-mode
;; (add-hook 'go-mode-hook (lambda ()
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (company-mode)
;;                           (flycheck-mode)))

;; https://github.com/syohex/emacs-go-eldoc
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; https://github.com/dominikh/go-errcheck.el
;; go get github.com/kisielk/errcheck
(require 'go-errcheck)

;; Add yasnippets-go:
;; https://github.com/dominikh/yasnippet-go

;; Fix tab size
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(server-start)

(provide 'gmbuell)

;;; gmbuell.el ends here
