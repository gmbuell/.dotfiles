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
(eval-when-compile
  (require 'use-package))

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

;; Replace all the search keys with regex versions.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(use-package saveplace
  :ensure t
  :init
  (setq-default save-place t)
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

;; From http://steckerhalter.co.vu/steckemacs.html
;; Make isearch-forward put the cursor at the start of the search, not the end.
(defun my-isearch-goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-isearch-goto-match-beginning)

(use-package highlight-symbol
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t))

(use-package iedit
  :ensure t
  :config
  (setq iedit-unmatched-lines-invisible-default t))

(use-package back-button
  :ensure t
  :init
  (back-button-mode 1))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :init
  ;; When doing isearch, hand the word over to helm-swoop
  (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; From helm-swoop to helm-multi-swoop-all
  (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map))

;; We need nested minibuffers mostly due to helm replacing "M-y"
(setq enable-recursive-minibuffers t)

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))


;; Ido configuration. Gives smart buffer switching and menus.
;; -------------------------------------------------------------------
(use-package recentf
  :ensure t
  :init
  (recentf-mode t)
  :config
  ;; Ignore some buffers.
  (add-to-list 'recentf-exclude "/COMMIT_EDITMSG$")
  (add-to-list 'recentf-exclude "/.ido.last$")
  (add-to-list 'recentf-exclude "^\*Helm"))

(use-package ido-hacks
  :ensure t
  :init
  (ido-mode t)
  (ido-ubiquitous-mode)
  :config
  (ido-hacks-mode)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching nil
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers nil
        ido-max-prospects 10
        ido-use-faces nil  ;; Disabled to see flx highlights
        )
  (add-to-list 'ido-ignore-buffers "^\*Helm")
  (add-to-list 'ido-ignore-buffers "^\*MULTI-TERM-DEDICATED\*"))
(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode t))

;; Ido uses "virtual buffers" so that you do not have to re-open files
;; between emacs sessions. This can get out of hand sometimes and this
;; command will clean everything up.
(defun ido-clear-history ()
  "Clear ido virtual buffers list."
  (interactive)
  (setq recentf-list '())
  (setq ido-virtual-buffers '()))

;; Smex is for smart completion of M-x commands.
(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  :init
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))



;; Find File at Point. Makes sevaral commands smarter when the cursor
;; is on something relevant.
(use-package ffap
  :ensure t
  :config
  (defvar ffap-c-commment-regexp "^/\\*+"
    "Matches an opening C-style comment, like \"/***\"."))

;; I have no idea where this came from.
;; (defadvice ffap-file-at-point (after avoid-c-comments activate)
;;   "Don't return paths like \"/******\" unless they actually exist.

;; This fixes the bug where ido would try to suggest a C-style
;; comment as a filename."
;;   (ignore-errors
;;     (when (and ad-return-value
;;                (string-match-p ffap-c-commment-regexp
;;                                ad-return-value)
;;                (not (ffap-file-exists-string ad-return-value)))
;;       (setq ad-return-value nil))))

;; Command to open files in another window. Useful for split buffer
;; setups.
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; Helm configuration. Helm is a great interface for finding things.
;; Protip: Helm uses space as a separator. So if you use helm to
;; switch to a buffer titled 'gmbuell-ruby.el', you could narrow using
;; "gm rub" and the space separator would start the other match
;; section. In regex speak, this is like space being translated to .*
;; This is unlike ido mode which transparently inserts virtual '.'
;; characters for matches.
(use-package helm-config
  :ensure helm
  :init
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map
                  [remap eshell-pcomplete]
                  'helm-esh-pcomplete)))
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (bind-key "M-p" 'helm-eshell-history eshell-mode-map)))
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (bind-key "<tab>" 'helm-esh-pcomplete eshell-mode-map)))
  (bind-key* "C-c C-b" 'helm-bookmarks)
  :bind (
         ;; This is my main interface to opening/switching between files in the
         ;; same git repository. It is particularly useful because the virtual
         ;; buffers of ido mode can make finding a project specific file
         ;; difficult.
         ("C-c h" . helm-ls-git-ls)
         ;; helm-ls-git-ls is specific to git projects. helm-browse-project
         ;; might be more general. Ex.
         ;; (global-set-key (kbd "C-c h") 'helm-browse-project)
         ("C-x C-b" . helm-buffers-list)
         ("C-c C-o" . helm-occur)
         ("C-c M-o" . helm-multi-occur)
         ("M-y" . helm-show-kill-ring)
         ;; Jump to a definition in the current file.
         ("C-x C-i" . helm-imenu))
  :config
  ;; Helm improvement. Make backspace quit when there is nothing
  ;; there instead of erroring.
  (defun helm-backspace ()
    "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
    (interactive)
    (condition-case nil
        (backward-delete-char 1)
      (error
       (helm-keyboard-quit))))
  (bind-key "DEL" 'helm-backspace helm-map)
  (set-default 'imenu-auto-rescan t))

(use-package helm-ls-git)

;; for eshell
(use-package pcomplete-extension
  :ensure t)

(use-package eshell
  :config
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-buffer-shorthand t
        pcomplete-ignore-case t
        eshell-save-history-on-exit t)
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH") "/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/gsutil"))
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))
  (global-set-key (kbd "C-c e") 'eshell-here)
  (defun delete-single-window (&optional window)
    "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
    (interactive)
    (save-current-buffer
      (setq window (or window (selected-window)))
      (select-window window)
      (kill-buffer)
      (if (one-window-p t)
          (delete-frame)
        (delete-window (selected-window)))))
  (defun eshell/x ()
    (delete-single-window))
  ;; Add pcomplete to company-capf
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'eshell-mode-hook #'add-pcomplete-to-capf)
  ;; Re-use eshell
  (defun af-eshell-here ()
    "Go to eshell and set current directory to the buffer's directory"
    (interactive)
    (let ((dir (file-name-directory (or (buffer-file-name)
                                        default-directory))))
      (eshell)
      (eshell/pushd ".")
      (cd dir)
      (goto-char (point-max))
      (eshell-kill-input)
      (eshell-send-input))))

(use-package esh-buf-stack
  :ensure t
  :config
  (setup-eshell-buf-stack)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key
               (kbd "M-q") 'eshell-push-command))))

(use-package pcmpl-pip
  :ensure t)

(use-package pcmpl-git
  :ensure t)

(use-package f
  :ensure t)

(defun pcmpl-blaze-target-cmd (target-type)
  "The blaze query command to run to get a list of targets."
  (cond ((string= target-type "test")
         "blaze query 'kind(\".*_test rule\", ...)'")
        ((string= target-type "all")
         "blaze query 'kind(\"rule\", ...)'")
        ((string= target-type "run")
         "blaze query 'kind(\".*_binary rule\", ...)'")))

(defun pcmpl-blaze-get-targets (target-type)
  "Return a list of `blaze' test targets."
  (let ((extraction-regexp (concat "//"
                                   (save-match-data
                                     (string-match ".+/google3/\\(.+\\)" (eshell/pwd))
                                     (match-string-no-properties 1 (eshell/pwd)))
                                   "\\(:.+\\)")))
    (with-temp-buffer
      (insert (shell-command-to-string (pcmpl-blaze-target-cmd target-type)))
      (goto-char (point-min))
      (let ((target-list))
        (while (re-search-forward extraction-regexp nil t)
          (add-to-list 'target-list (match-string-no-properties 1)))
        target-list))))

(defconst pcmpl-blaze-commands
  '("build" "test" "run")
  "List of `blaze' commands.")
(defun pcomplete/blaze ()
  "Completion for `blaze'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-blaze-commands)

  ;; Complete targets
  (cond
   ((pcomplete-match "test")
    (while (< pcomplete-index pcomplete-last)
      (pcomplete-next-arg))
    (pcomplete-here* (pcmpl-blaze-get-targets "test")))
   ((pcomplete-match "run")
    (while (< pcomplete-index pcomplete-last)
      (pcomplete-next-arg))
    (pcomplete-here* (pcmpl-blaze-get-targets "run")))))

(defun pcomplete/iwyu.py ()
  "Completion for `iwyu'."
  ;; Complete targets
  (while (< pcomplete-index pcomplete-last)
    (pcomplete-next-arg))
  (pcomplete-here* (pcmpl-blaze-get-targets "all")))

(defun pcomplete/fixdeps_main.par ()
  "Completion for `fixdeps'."
  ;; Complete targets
  (while (< pcomplete-index pcomplete-last)
    (pcomplete-next-arg))
  (pcomplete-here* (pcmpl-blaze-get-targets "all")))

;; Consider giving helm-adaptative-mode a try

;; Plain text settings
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Try and use aspell but fall back to ispell
'(if (executable-find "aspell")
     (progn
       (setq ispell-program-name "aspell")
       (setq ispell-list-command "list")
       (use-package aspell))
   (when (executable-find "ispell")
     (setq ispell-program-name "ispell")
     (use-package ispell)))

(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'markdown-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil))

;; Emacs expects sentences to end with double spaces. This is crazy.
(setq sentence-end-double-space nil)

;; Code health
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(column-number-mode t)

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
;; (font-lock-add-keywords 'go-mode (font-lock-width-keyword 80))

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
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  ;; Maybe fix flycheck and company mode interaction. Is this even a
  ;; problem?
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil)))

;; Company mode for completions: http://company-mode.github.io/
(use-package company
  :ensure t
  :bind ("M-/" . company-complete)
  :init
  (global-company-mode)
  :config
  ;; Add company-yasnippet carefully.
  (setq company-backends (-map (lambda (item) (cond ((listp item)
                                                     (cons 'company-yasnippet item))
                                                    ((eq 'company-dabbrev item)
                                                     '(company-yasnippet company-dabbrev))
                                                    (t item
                                                       ))) company-backends))
  (add-to-list 'company-backends 'company-warhammer)
  ;; Decrease delay before autocompletion popup shows.
  (setq company-idle-delay .3)
  ;; Bigger popup window.
  (setq company-tooltip-limit 20)
  (setq company-echo-delay 0))

(use-package ycmd
  :ensure t
  :init
  (set-variable 'ycmd-server-command '("/usr/grte/v4/bin/python2.7" "/usr/lib/youcompleteme/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "/usr/lib/youcompleteme/ycm_extra_conf.py")
  (set-variable 'ycmd-extra-conf-whitelist '("/usr/lib/youcompleteme/ycm_extra_conf.py"))
  (set-variable 'ycmd-parse-conditions '(save new-line mode-enabled))
  (set-variable 'url-show-status nil)
  (global-ycmd-mode))

(use-package ycmd-next-error
  :ensure ycmd)

(use-package company-ycmd
  :ensure t
  :init
  (company-ycmd-setup)
  ;; 'company-clang' isn't needed with company-ycmd.
  (delq 'company-clang company-backends))

(use-package flycheck-ycmd
  :ensure t
  :init
  (flycheck-ycmd-setup)
  :config
  (setq-default flycheck-disabled-checkers
                '(c/c++-gcc c/c++-clang c/c++-cppcheck)))

;; https://github.com/nsf/gocode/tree/mast~/gocodeer/emacs-company
;; go get -u github.com/nsf/gocode
(use-package company-go
  :ensure t)

;; Git integration
;; -------------------------------------------------------------------
;; Shows git diff information in the gutter.
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t))

;; Magit makes git inside emacs awesome.
;; Start with "C-c g"
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  (global-auto-revert-mode)
  (setq auto-revert-check-vc-info t)
  (setq vc-follow-symlinks t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (add-to-list 'sml/hidden-modes magit-auto-revert-mode-lighter))

(use-package multi-term
  :ensure t
  :bind (("<f1>" . multi-term-dedicated-toggle)
         ("C-c s" . multi-term-dedicated-toggle))
  :config
  (bind-key "C-y" 'term-paste term-raw-map)
  (setq
   multi-term-dedicated-select-after-open-p t
   ;; Use zsh for multi-term
   multi-term-program "/usr/local/bin/zsh"
   multi-term-program-switches "--login"
   multi-term-dedicated-skip-other-window-p t))

(use-package ansi-color
  :ensure t
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-prompt-read-only t))

;; Sets up Shift + arrow keys for moving between frames and windows.
;; This is particularly useful for multi-monitor setups.
(use-package framemove
  :ensure t
  :init
  (windmove-default-keybindings)
  :config
  (setq framemove-hook-into-windmove t))

;; Deft is my preferred note-taking setup. See:
;; http://jblevins.org/projects/deft/
(use-package deft
  :ensure t
  :config
  ;; Set the deft directory to Dropbox/notes if it exists.
  (when (file-exists-p "~/Dropbox")
    (setq deft-directory "~/Dropbox/notes")
    (when (not (file-exists-p deft-directory))
      (make-directory deft-directory t)))
  (setq deft-text-mode 'org-mode)
  (setq deft-extension "org")
  (setq deft-use-filename-as-title t))

;; eww browsing inside emacs. Much better than w3m!
(setq browse-url-browser-function 'eww-browse-url)

;; Google Talk inside Emacs.
(use-package jabber
  :ensure t
  :config
  (setq jabber-account-list
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
        jabber-history-dir "~/.jabber")
  (set-face-attribute 'jabber-chat-prompt-local nil
                      :foreground "#195466")
  (set-face-attribute 'jabber-chat-prompt-foreign nil
                      :foreground "#c23127")
  (set-face-attribute 'jabber-roster-user-xa nil
                      :foreground "#0a3749")
  (set-face-attribute 'jabber-roster-user-online nil
                      :foreground "#599cab")
  (set-face-attribute 'jabber-roster-user-away nil
                      :foreground "#245361")
  (set-face-attribute 'jabber-activity-face nil
                      :foreground "#d26937")
  (set-face-attribute 'jabber-activity-personal-face nil
                      :foreground "#c23127"))

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

(defun gmbuell-smart-kill-line (arg)
  "If the line is only whitespace or the command is prefixed with C-u,
   use standard kill-line. Otherwise, use sp-kill-hybrid-sexp"
  (interactive "P")
  (if (or arg (string-blank-p (thing-at-point 'line)))
      (progn (kill-line nil)
             (indent-for-tab-command))
    (progn (sp-kill-hybrid-sexp arg)
           (indent-for-tab-command))))

(defun gmbuell-smartparens-config ()
  "Custom configuration for smartparens."
  (setq sp-override-key-bindings
        '(("C-<right>" . sp-slurp-hybrid-sexp)
          ("C-<left>" . sp-dedent-adjust-sexp)))
  ;; Fix forward slurp spacing
  ;; https://github.com/Fuco1/smartparens/issues/297
  (sp-local-pair 'c-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*")
  (sp-local-pair 'c++-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*)")
  (sp-local-pair 'go-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*")
  (require 'subr-x)
  ;; My custom code to make kill line more intelligent
  (bind-key "C-k" 'gmbuell-smart-kill-line prog-mode-map)
  (add-hook 'c++-mode-hook
            '(lambda () (bind-key "C-k" 'gmbuell-smart-kill-line c++-mode-map)))
  (add-hook 'c-mode-hook
            '(lambda () (bind-key "C-k" 'gmbuell-smart-kill-line c-mode-map)))
  (add-hook 'go-mode-hook
            '(lambda () (bind-key "C-k" 'gmbuell-smart-kill-line go-mode-map))))

(use-package smartparens-config
  :ensure smartparens
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode +1)
  (sp-use-smartparens-bindings)
  (gmbuell-smartparens-config))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Give visual feedback on some commands
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Show search progress
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :config
  (setq
   anzu-mode-lighter nil
   anzu-deactivate-region t
   anzu-search-threshold 1000
   anzu-replace-to-string-separator " => "))

;; ("http://www.cardgamedb.com/forums/index.php?/rss/ccs/1c61-Game%20of%20Thrones/")
;; (setq
;;  elfeed-feeds
;;  (quote
;;   ("http://gaijinhunter.tumblr.com/rss"))
;;  shr-blocked-images ".+")

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'sql-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode))

;; Miscellaneous modes
;; -------------------------------------------------------------------

;; Enhancements to dired including dired-jump
(use-package dired-x
  :init
  (setq dired-bind-jump nil)
  :bind ("C-x j" . dired-jump))

(use-package dash
  :ensure t
  :config
  (dash-enable-font-lock))

(use-package coffee-mode
  :ensure t
  :config
  (bind-key "C-c C-c" 'coffee-compile-file coffee-mode-map)
  (setq coffee-tab-width 2))

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
;; (global-set-key (kbd "C-g") 'top-level)

;; For chromebook:
(global-set-key (kbd "<deletechar>") 'backward-kill-word)

;; Register
;; (defun copy-to-j (start end)
;;   "Copy the text in the region to register 'j'."
;;   (interactive "r")
;;   (copy-to-register ?j start end))
;; (defun paste-from-j ()
;;   "Paste the text from register 'j'."
;;   (interactive)
;;   (insert-register ?j t))
;; (define-key global-map (kbd "C-c C-j") 'copy-to-j)
;; (define-key global-map (kbd "C-j") 'paste-from-j)

;; Add zsh history search to helm
;; (defvar helm-c-source-zsh-history
;;   '((name . "Zsh History")
;;     (candidates . helm-c-zsh-history-set-candidates)
;;     (action . (("Execute Command" . helm-c-zsh-history-action)))
;;     (volatile)
;;     (requires-pattern . 3)
;;     (delayed)))

;; (defun helm-c-zsh-history-set-candidates (&optional request-prefix)
;;   (let ((pattern (replace-regexp-in-string
;;                   " " ".*"
;;                   (or (and request-prefix
;;                            (concat request-prefix
;;                                    " " helm-pattern))
;;                       helm-pattern))))
;;     (with-current-buffer (find-file-noselect "~/.zsh_history" t t)
;;       (auto-revert-mode -1)
;;       (goto-char (point-max))
;;       (loop for pos = (re-search-backward pattern nil t)
;;             while pos
;;             collect (replace-regexp-in-string
;;                      "\\`:.+?;" ""
;;                      (buffer-substring (line-beginning-position)
;;                                        (line-end-position)))))))

;; (defun helm-c-zsh-history-action (candidate)
;;   (multi-term-dedicated-select)
;;   (kill-new candidate)
;;   (yank))

;; (defun helm-command-from-zsh ()
;;   (interactive)
;;   (helm-other-buffer 'helm-c-source-zsh-history "*helm zsh history*"))

;; (define-key global-map (kbd "C-c C-z") 'helm-command-from-zsh)

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "gfm"))


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
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'respectful)
  (sml/setup)
  :config
  (setq sml/mode-width (quote full)
        sml/mule-info nil
        sml/position-percentage-format nil
        sml/show-remote nil
        sml/size-indication-format "")
  (add-to-list 'sml/hidden-modes " GitGutter"))

;; Nice fonts:
;; Hack https://github.com/chrissimpkins/Hack
;; Inconsolata-11
;; Droid Sans Mono-11
;; DejaVu Sans Mono-11
;; Note, these require: apt-get install ttf-droid ttfinconsolata
;; ttf-dejavu
;; Only load the theme if we are in a graphical display.
(when (display-graphic-p)
  (use-package gotham-theme
    :ensure t
    :init
    (load-theme 'gotham t))
  (set-frame-font "Inconsolata 12" t t))


;; My pinky hurts. Lets try out ace-jump-mode.
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (ace-jump-mode-enable-mark-sync)
  (setq ace-jump-mode-scope 'window)
  (add-hook 'eshell-mode-hook
            '(lambda () (bind-key "C-c SPC" 'ace-jump-mode eshell-mode-map))))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; go-mode
;; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;; http://dominik.honnef.co/posts/2013/08/writing_go_in_emacs__cont__/
(use-package go-mode
  :ensure t)
;; https://github.com/dougm/goflymake
;; go get -u github.com/dougm/goflymake
(use-package go-flycheck
  :load-path (lambda () (concat (getenv "HOME") "/go/src/github.com/dougm/goflymake")))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (flycheck-mode)))

;; https://github.com/syohex/emacs-go-eldoc
(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; https://github.com/dominikh/go-errcheck.el
;; go get github.com/kisielk/errcheck
(use-package go-errcheck)

;; Add yasnippets-go:
;; Isn't needed since the default yasnippet directory contains some.
;; https://github.com/dominikh/yasnippet-go
;; (add-to-list 'yas-snippet-dirs (substitute-in-file-name "$HOME/github/yasnippet-go"))

;; Fix tab size
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; Empty scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Correct words and add to abbrev for auto correction
;; (define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(use-package re-builder
  :ensure t
  :config (setq reb-re-syntax 'string))

;; Uses expand-region for movement
;; Note, this doesn't work as well as one would hope...
;; (require 'smart-forward)
;; (global-set-key (kbd "M-p") 'smart-up)
;; (global-set-key (kbd "M-n") 'smart-down)
;; (global-set-key (kbd "M-b") 'smart-backward)
;; (global-set-key (kbd "M-f") 'smart-forward)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


(use-package org
  :config
  (bind-key "C-c SPC" 'ace-jump-mode org-mode-map)
  ;; Add shortcut to recalculate table
  (bind-key "M-r" '(lambda () (interactive)(org-table-recalculate t)) org-mode-map))

;; Try out hydra:
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

(use-package multifiles
  :ensure t
  :bind ("C-!" . mf/mirror-region-in-multifile))

(defun keys-describe-prefixes ()
  (interactive)
  (with-output-to-temp-buffer "*Bindings*"
    (dolist (letter-group (list
                           (cl-loop for c from ?a to ?z
                                    collect (string c))))
      (dolist (prefix '("" "C-" "M-" "C-M-"))
        (princ (mapconcat
                (lambda (letter)
                  (let ((key (concat prefix letter)))
                    (format ";; (global-set-key (kbd \"%s\") '%S)"
                            key
                            (key-binding (kbd key)))))
                letter-group
                "\n"))
        (princ "\n\n")))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

(use-package multiple-cursors
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Faster tramp startup
(setq tramp-default-method "ssh")

;; Emacs doesn't understand objective-c++
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(defun find-experiment ()
  (interactive)
  (save-excursion
    (er/mark-symbol)
    (let ((file-name (car (last (s-match "^/google/src/files/head/depot/google3/\\([^ ]+\\)"
                                         (shell-command-to-string (concat "cs f:gcl " (buffer-substring-no-properties (region-beginning) (region-end)))))))))
      (move-end-of-line nil)
      (insert file-name)
      )))

(use-package aggressive-indent
  :ensure t
  ;; :init
  ;; (global-aggressive-indent-mode 1)
  )

(use-package smartscan
  :ensure t
  :init
  (smartscan-mode 1))

(use-package auto-yasnippet
  :ensure t
  :bind (("M-w" . aya-create)
         ("M-W" . aya-expand)))

;; Protocol buffer support
(use-package protobuf-mode
  :ensure t)

(use-package flycheck-protobuf
  :ensure t)

;; Show/Hide images in eww
(defvar-local endless/display-images t)

(defun endless/toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq endless/display-images
        (null endless/display-images))
  (endless/backup-display-property endless/display-images))
(defun endless/backup-display-property (invert &optional object)
  "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer. Nil means current
buffer."
  (let* ((inhibit-read-only t)
         (from (if invert 'display-backup 'display))
         (to (if invert 'display 'display-backup))
         (pos (point-min))
         left prop)
    (while (and pos (/= pos (point-max)))
      (if (get-text-property pos from object)
          (setq left pos)
        (setq left (next-single-property-change pos from object)))
      (if (or (null left) (= left (point-max)))
          (setq pos nil)
        (setq prop (get-text-property left from object))
        (setq pos (or (next-single-property-change left from object)
                      (point-max)))
        (when (eq (car prop) 'image)
          (add-text-properties left pos (list from nil to prop) object))))))

(server-start)

(provide 'gmbuell)

;;; gmbuell.el ends here
