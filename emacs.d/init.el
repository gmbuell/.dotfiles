(setq gc-cons-threshold 100000000)
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always load newest byte code
(setq load-prefer-newer t)

(require 'use-package)
(setq use-package-verbose t)
(use-package auto-package-update
  :ensure t
  :config
  ;;  Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;;Update installed packages at startup if there is an update pending.
  :init (auto-package-update-maybe))

;; Add quelpa for an additional way of adding packages
(use-package quelpa
  :ensure t
  :init
  (setq quelpa-upgrade-p t))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; (eval-when-compile
;;   (require 'use-package))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; Disable graphical garbage
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
   (scroll-bar-mode -1))
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (wgrep region-bindings-mode mosey ivy-hydra flycheck-inline breadcrumb fold-this phi-replace phi-search flycheck-eglot shell-pop ycmd-next-error company-quickhelp dired-x dired uniquify dumb-jump pretty-hydra posframe hydra-posframe quelpa-use-package quelpa clipetty nov ivy-xref ycmd-eldoc monky edit-server multifiles discover-my-major help-fns+ golden-ratio beginend flycheck-golangci-lint godoctor company-go rainbow-delimiters easy-kill eglot which-key use-package term-projectile stickyfunc-enhance srefactor smex smartscan smartparens smart-mode-line protobuf-mode origami multiple-cursors magit js2-mode highlight-symbol headlong google-c-style go-errcheck go-eldoc git-gutter ghub function-args flycheck-ycmd find-file-in-project expand-region eterm-256color diminish deft counsel-projectile company-ycmd company-statistics color-theme-solarized base16-theme avy auto-yasnippet)))
 '(sp-override-key-bindings
   (quote
    (("C-<right>" . sp-slurp-hybrid-sexp)
     ("C-<left>" . sp-dedent-adjust-sexp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------
;; Auto refresh
;; Global auto revert seems to hang things?
;; (global-auto-revert-mode 1)
(global-auto-revert-mode -1)
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (auto-revert-mode 1)))
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

(use-package dired
  :config
  ;; Make dired "guess" target directory for some operations, like copy to
  ;; directory visited in other split buffer.
  (setq dired-dwim-target t)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (require 'dired-x)
  (setq dired-omit-mode t))

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar spacemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar spacemacs-useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching
`spacemacs-useless-buffers-regexp'.")

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; ---------------------------------------------------------------------------
;; Edit
;; ---------------------------------------------------------------------------

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 4)
(set-default 'indicate-empty-lines t)

;; Text
(setq longlines-show-hard-newlines t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)

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

;; persistent abbreviation file
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Better cut/paste handling for ssh and tmux
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

;; Show column number in mode line
(setq column-number-mode t)

;; highlight current line
;; (global-hl-line-mode t)
;; no blink
(blink-cursor-mode 0)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq ns-use-native-fullscreen t)

;; scratch buffer empty
(setq initial-scratch-message nil)
;; don't create backup~ files
(setq make-backup-files nil)

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(require 'bookmark)

;; (setq whitespace-style '(face trailing lines-tail tabs)
;;       whitespace-line-column 80
;;       save-place-file (concat user-emacs-directory "places"))

;; Smex is for smart completion of M-x commands.
(use-package smex
  :ensure t
  ;; :bind  (("M-x" . smex)
  ;;         ("M-X" . smex-major-mode-commands))
  :init
  (progn
    (setq-default smex-history-length 32
                  smex-save-file (concat user-emacs-directory
                                         ".smex-items"))
    (smex-initialize)))

;; Enable virtual buffers
(recentf-mode 1)
(setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                        "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                        ".*png$" ".*cache$"))

;; For fuzzy searching.
(use-package flx
  :ensure t)

;; Look at these release notes to see other cool ivy stuff
;; https://oremacs.com/2016/04/26/ivy-0.8.0/
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :init (progn
          (ivy-mode 1)
          (setq ivy-extra-directories nil)
          (setq ivy-use-virtual-buffers t)
          (setq enable-recursive-minibuffers t)
          ;; FYI, M-r will disable fuzzy matching for a single search.
          ;; I've used ivy--regex-fuzzy in the past but it's a disaster too many places.
          (setq ivy-re-builders-alist
                '((counsel-M-x . ivy--regex-fuzzy)
                  (counsel-find-file . ivy--regex-fuzzy)
                  (ivy-switch-buffer . ivy--regex-fuzzy)
                  (counsel-git . ivy--regex-fuzzy)
                  (t . ivy--regex-plus)))
          ;; Get rid of initial ^ to make everything fuzzy.
          (setq ivy-initial-inputs-alist nil)
          ;; This solves the long standing issue of e.g. creating a file or a directory
          ;; foo when a file foobar already exists. Previously, the only solution was to
          ;; use C-M-j. It's still available, but now you can also select your input with
          ;; C-p and press RET.
          (setq ivy-use-selectable-prompt t)
          ))

(use-package ivy-hydra
  :after (ivy)
  :ensure t)

;; Consider also occur mode
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L477-L483
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L938-L947
;; https://oremacs.com/2015/01/26/occur-dwim/
;; https://github.com/abo-abo/hydra/wiki/Emacs

(use-package swiper
  :ensure t
  ;; :bind (("C-s" . counsel-grep-or-swiper))
  ;; :bind (("C-s" . swiper-isearch))
  )
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (;; ("M-i" . counsel-semantic-or-imenu)
         ;; Using function-args instead.
         ("C-c h" . counsel-git))
  :init
  (counsel-mode 1)
  ;; :bind (("M-x" . counsel-M-x)
  ;;        ("C-x C-f" . counsel-find-file)
  ;;        :map minibuffer-local-map
  ;;        ("C-r" . counsel-minibuffer-history))
  )

;; Lets try using ripgrep (rg) for everything
(setq counsel-git-cmd "rg --files -0")
(setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")
(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")
;; I've also seen this command recommended. Not sure if it works.
;; (setq counsel-grep-base-command
;;     "rg -i -M 120 --no-heading --line-number --color never %s %s")

;; Git integration
;; -------------------------------------------------------------------
;; Shows git diff information in the gutter.
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))

;; Magit makes git inside emacs awesome.
;; Start with "C-c g"
;; http://daemianmack.com/magit-cheatsheet.html
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  ;; (global-auto-revert-mode)
  (setq auto-revert-check-vc-info t)
  (setq vc-follow-symlinks t))

(use-package monky
  :ensure t
  ;; :bind ("C-c g" . monky-status)
  :init
  ;; By default monky spawns a seperate hg process for every command.
  ;; This will be slow if the repo contains lot of changes.
  ;; if `monky-process-type' is set to cmdserver then monky will spawn a single
  ;; cmdserver and communicate over pipe.
  ;; Available only on mercurial versions 1.9 or higher
  ;;(setq monky-process-type 'cmdserver)
  )

;; Replace all the search keys with regex versions.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

(defalias 'auto-tail-revert-mode 'tail-mode)

(use-package saveplace
  :ensure t
  :init
  (save-place-mode +1)
  (setq select-enable-clipboard t
        select-enable-primary t
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
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t))

(add-hook 'prog-mode (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace t)))

(set-default 'imenu-auto-rescan t)
;; Teach imenu to recognize TEST_F
(add-hook 'c++-mode
          (lambda ()
            (when (s-ends-with? "_test.cc" buffer-file-name)
              (setq-local
               imenu-generic-expression
               '(("Class" "^\\(template[        ]*<[^>]+>[      ]*\\)?\\(class\\|struct\\)[     ]+\\([[:alnum:]_]+\\(<[^>]+>\\)?\\)\\([         \n]\\|\\\\\n\\)*[:{]" 3)
                 ("Test" "^ *TEST\\(?:_F\\)?([^,]+,\n? *\\(.+\\)) {$" 1))))))

(use-package stickyfunc-enhance
  :after (semantic)
  :ensure t
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;;(add-hook 'prog-mode-hook #'semantic-mode))
 )

;; (use-package cedet)

(use-package semantic
  :init
  ;; select which submodes we want to activate
;;  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;;  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;;  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
;;  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  ;; Activate semantic
;;  (semantic-mode 1)
;;  (semanticdb-enable-gnu-global-databases 'c-mode t)
;;  (semanticdb-enable-gnu-global-databases 'c++-mode t)
;;  (when (cedet-ectag-version-check t)
;;    (semantic-load-enable-primary-ectags-support))
)

;; srefactor-refactor-at-point
;; (use-package srefactor
;;   :after (semantic)
;;   :ensure t
;;   :bind (:map c-mode-map
;;               ("M-RET" . srefactor-refactor-at-point)
;;               :map c++-mode-map
;;               ("M-RET" . srefactor-refactor-at-point)
;;               ))

;; Deft is my preferred note-taking setup. See:
;; http://jblevins.org/projects/deft/
(use-package deft
  :ensure t
  :init
  (progn
    ;; Set the deft directory to Dropbox/notes if it exists.
    (when (file-exists-p "~/Dropbox")
      (setq deft-directory "~/Dropbox/notes")
      (when (not (file-exists-p deft-directory))
        (make-directory deft-directory t)))
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filter-string-for-filename t)
    (setq deft-use-filename-as-title t)))

;; eww browsing inside emacs. Much better than w3m!
(setq browse-url-browser-function 'eww-browse-url)

(require 'subr-x)

;; Smartparens
(defun gmbuell-smart-kill-line (arg)
  "If the line is only whitespace or the command is prefixed with C-u,
   use standard kill-line. Otherwise, use sp-kill-hybrid-sexp"
  (interactive "P")
  (if (or arg (string-blank-p (thing-at-point 'line)))
      (progn (kill-line nil)
             (indent-for-tab-command))
    (progn (sp-kill-hybrid-sexp arg)
           (indent-for-tab-command))))

(use-package cc-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :bind (:map prog-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map c++-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map c-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map go-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              )
  :init (progn
          (smartparens-global-mode t)
          (show-smartparens-global-mode +1)
          (sp-use-smartparens-bindings)
          (custom-set-variables '(sp-override-key-bindings
                                  '(("C-<right>" . sp-slurp-hybrid-sexp)
                                    ("C-<left>" . sp-dedent-adjust-sexp))))
          ;; Fix forward slurp spacing
          ;; https://github.com/Fuco1/smartparens/issues/297
          (sp-local-pair 'c-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*")
          (sp-local-pair 'c++-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*)")
          (sp-local-pair 'go-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

  ;; For chromebook:
  (global-set-key (kbd "<deletechar>") 'backward-kill-word))

;; Look at https://github.com/Fuco1/smartparens/issues/209 for ideas for other ways to use smartparns for movement.
(use-package mosey
  :ensure t
  :init
  (defmosey '(beginning-of-line
              back-to-indentation
              sp-backward-sexp  ; Moves across lines
              sp-forward-sexp   ; Moves across lines
              mosey-goto-end-of-code
              mosey-goto-beginning-of-comment-text
              end-of-line)
    :prefix "c")
  :bind* (("C-a" . mosey-backward-bounce)
         ("C-e" . mosey-forward-bounce)
         )
  )

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
  (add-to-list 'sml/hidden-modes " GitGutter")
  (add-to-list 'sml/hidden-modes " ARev"))

;; Nice fonts:
;; Hack https://github.com/chrissimpkins/Hack
;; Inconsolata-11
;; Droid Sans Mono-11
;; DejaVu Sans Mono-11
;; Note, these require: apt-get install ttf-droid ttfinconsolata
;; ttf-dejavu
;; Only load the theme if we are in a graphical display.
  ;; (use-package gotham-theme
  ;;   :ensure t
  ;;   :init
;;   (load-theme 'gotham t))
;; (use-package color-theme
;;   :if window-system
;;   :ensure t)
;; monokai
;; spacemacs
;; railscasts
;; chalk
(use-package base16-theme
  ;; :if window-system
  :ensure t
  :init
  (setq base16-theme-256-color-source 'colors)
  (load-theme 'base16-monokai t))
(when (display-graphic-p)
  (set-frame-font "Inconsolata 12" t t))

(use-package re-builder
  :ensure t
  :config (setq reb-re-syntax 'string))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (setq yas-triggers-in-field t))

;; Disable abbrev-mode
(setq-default abbrev-mode -1)
(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode -1)))

;; (use-package org
;;   :ensure t
;;   :init
;;   (setq org-confirm-babel-evaluate nil
;;       org-src-fontify-natively t
;;       org-src-tab-acts-natively t)
;;   :config
;;   ;; (bind-key "C-c SPC" 'ace-jump-mode org-mode-map)
;;   ;; Add shortcut to recalculate table
;;   ;; (bind-key "M-r" '(lambda () (interactive)(org-table-recalculate t)) org-mode-map)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((sh . t)
;;      (comint . t)
;;      (sql . t)
;;      ;;(dremel . t)
;;      )))

;; Faster tramp startup
;;(setq tramp-default-method "ssh")


;; M-n and M-p move between symbols
;; M-' to replace all symbols in the buffer matching the one under point
;; C-u M-' to replace symbols in your current defun only (as used by narrow-to-defun.)
(use-package smartscan
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; Completion
;; Exits the minibuffer as soon as there is a unique candidate
(use-package headlong
  :ensure t
  :defer t)

(defadvice custom-theme-load-confirm (around no-query-safe-thme activate)
  t)

(use-package dired
  :commands dired)
(use-package dired-x
  :commands dired-jump)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-word-1)
         ("C-c SPC" . avy-goto-word-1)
         ;;("C-c C-j" . avy-resume)
         )
  :init (progn
          (avy-setup-default)
          (setq avy-style 'at)
          (setq avy-background t)
          (setq avy-keys (number-sequence ?a ?z))))

;; Project managment
;; Also look into bufler for this
;; https://github.com/alphapapa/bufler.el#compared-to-ibuffer
(use-package projectile
  :ensure t
  :init (progn
          (projectile-global-mode)
          (setq projectile-indexing-method 'alien)
          (setq projectile-completion-system 'ivy)))
(setq projectile-git-command "fd . -t f -0")
(setq projectile-generic-command "fd . -t f -0")
(use-package find-file-in-project
  :ensure t
  :commands find-file-in-project)
(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :init (counsel-projectile-mode)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package f
  :ensure t)

(defun refresh-projectile ()
  (interactive)
  "Reset projectile known projects to citc and git5 clients."
  (projectile-clear-known-projects)
  (mapc #'projectile-add-known-project
        (f-glob "~/Code/*/google3"))
  (mapc #'projectile-add-known-project
        (seq-remove (apply-partially 's-contains? "git5") (f-glob "/google/src/cloud/gmbuell/*/google3")))
  (projectile-save-known-projects))

(projectile-register-project-type 'google3 '("WORKSPACE"))

(refresh-projectile)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;; Allow company completions by selecting the completion number
(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))



(use-package company
  :ensure t
  ;;:bind ([remap indent-for-tab-command] . company-indent-or-complete-common)
  :bind (:map prog-mode-map
         ("TAB" . company-indent-or-complete-common))
  :init
  (global-company-mode)
  (setq company-tooltip-limit 9)                      ; bigger popup window
  (setq company-idle-delay .5)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-require-match nil)                     ;  Allow exiting the completion
  (setq company-show-numbers t)
  (setq company-backends
        (quote
         ((company-ycmd company-capf)
          company-dabbrev-code)))
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  ;; (setq company-backends
  ;;       (quote
  ;;        ((company-ycmd company-capf) company-semantic company-files
  ;;         (company-dabbrev-code company-gtags company-etags))))
  ;; If completing on . or -> is too aggressive:
  ;; (setq company-begin-commands '(self-insert-command))
  ;; Remove company-clang since we don't use it.
  ;;(delq 'company-clang company-backends)
  ;; dabbrev is probably evil when there are reasonable completion methods setup.
  ;;(delq 'company-dabbrev company-backends)
  )
(setq tab-always-indent 'complete)

(use-package company-quickhelp
  :ensure t
  :init (add-hook 'after-init-hook 'company-quickhelp-mode))

(use-package company-statistics
  :after (company)
  :ensure t
  :init
  (company-statistics-mode))

;; C++
(use-package function-args
  :ensure t
  :bind (("M-i" . moo-jump-local))
  :init
  (fa-config-default)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (set-default 'semantic-case-fold t)
  ;; (semantic-add-system-include "/google/src/components/head/google3" 'c++-mode)
  )
;; 

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
     :post (quit-windows-on "*Flycheck errors*")
     :hint nil)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "Next")
  ("k" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))

(use-package flycheck-inline
  :after (flycheck)
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package google-c-style
  :ensure t
  :init (progn
          (add-hook 'c-mode-common-hook 'google-set-c-style)
          (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

;; Stop indenting in namespaces
(c-set-offset 'innamespace 0)

(use-package eglot
  :after (company yasnippet)
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'eglot-ensure)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  ;; kythe is deprecated but it works on unsubmitted code.
  (add-to-list
   'eglot-server-programs
   '((c++-mode c-mode java-mode python-mode protobuf-mode)
     . ("/google/bin/releases/grok/tools/kythe_languageserver" "--google3"))))

(use-package ivy-xref
  :after (ivy)
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(require 'nadvice)
(defun do-nothing (orig-fun &rest args)
  t)

;; This shows function arguments in the minibuffer which is very noisy.
;; (use-package eldoc)

;; It may be possible to use avy to set marks for multiple cursors.
;; I.e. avy-push-mark
;; Also via swiper https://oremacs.com/2015/10/14/swiper-mc/
(use-package multiple-cursors
  :ensure t
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-symbols-like-this-in-defun)))

(use-package region-bindings-mode
  :after (multiple-cursors)
  :ensure t
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("m" . mc/mark-more-like-this-extended)
              ("f" . mc/skip-to-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("u" . er/contract-region)
              ("h" . mc-hide-unmatched-lines-mode))
  :init
  (region-bindings-mode-enable))

(use-package fold-this
  :ensure t
  :bind (
         ("C-c C-f" . fold-active-region-all)))

;; phi search works with multiple cursors
;; But has weird behavior around trying to move while in a search
(use-package phi-search
  :ensure t)
(use-package phi-replace
  ;; Doesn't have it's own package.
  :after (phi-search))
;;   :bind (
;;          ("C-s" . phi-search)
;;          ("C-r" . phi-search-backward)))
;; (use-package phi-replace
;;   ;; Doesn't have it's own package.
;;   :after (phi-search)
;;   :bind (
;;          ("M-%" . phi-replace-query)))

(use-package multifiles
  :ensure t
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package auto-yasnippet
  :after (yasnippet)
  :ensure t
  :bind (("M-w" . aya-create)
         ("M-W" . aya-expand)))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Protocol buffer support
(use-package protobuf-mode
  :ensure t)

(use-package google-borg-helpers :load-path "/usr/share/emacs/site-lisp/emacs-google-config/third_party/elisp/google_borg_helpers/")
(use-package borg-mode :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/")

;; (use-package google3-eglot :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/"
;;   :after (eglot company)
;;   :init
;;   (google3-eglot-setup))

;; (use-package google-kythe :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/kythe"
;;   :after (eglot))
;; (use-package google-lsp :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/"
;;   :after (eglot company)
;;   :init
;;   (google-lsp-init))

;; My custom hacked CiderLSP support
;; (add-to-list
;;    'eglot-server-programs
;;    '((c++-mode c-mode java-mode python-mode protobuf-mode)
;;      . ("/google/bin/releases/editor-devtools/ciderlsp" "--noforward_sync_responses" "--request_options=enable_placeholders" "--tooltag=emacs-eglot")))

(use-package ycmd
  :ensure t
  :init
  (global-ycmd-mode)
  :config
  (setq ycmd-global-config "/usr/lib/youcompleteme/ycm_extra_conf.py")
  (setq ycmd-server-command
        '("/usr/grte/v4/bin/python2.7"
          "/usr/lib/youcompleteme/third_party/ycmd/ycmd"))
  (add-to-list 'ycmd-extra-conf-whitelist "/usr/lib/youcompleteme/ycm_extra_conf.py")
  (setq ycmd-startup-timeout 20000)
  ;; (setq ycmd-idle-change-delay 0.5)
  (setq company-minimum-prefix-length 2))

(use-package company-ycmd
  :load-path "/usr/local/google/home/gmbuell/.emacs.d/lisp/"
  :after (ycmd company)
  ;; :ensure t
  :init
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :after (ycmd flycheck)
  :ensure t
  :init
  (flycheck-ycmd-setup))

(use-package flycheck-eglot
  :load-path "/usr/local/google/home/gmbuell/.emacs.d/lisp/"
  :after (eglot flycheck)
  :init
  (add-to-list 'eglot-stay-out-of 'company))

;; This appears to make the minibuffer very busy
;; (use-package ycmd-eldoc
;;   :after (ycmd eldoc)
;;   :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

(use-package ycmd-next-error
  :after (ycmd))

(defhydra hydra-next-error
  (global-map "C-x")
  "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
  ("`" next-error     nil)
  ("j" next-error     nil :bind nil)
  ("k" previous-error nil :bind nil)
  ("h" first-error    nil :bind nil)
  ("l" (condition-case err
           (while t
             (next-error))
         (user-error nil))
   nil :bind nil)
  ("q" nil            nil :color blue))


;; (use-package p4
;;   :ensure t)
;; (use-package p4-google :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/")
;; (use-package citc :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/")
;; (use-package google3-build-capf :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/"
;;   :init
;;   (google3-build-capf-enable-completions))

(setq auto-mode-alist
      (nconc
       (list
        (cons "\\.pyplan\\'" 'python-mode)
        ;; MPM package definition files are Python syntax.
        (cons "/pkgdef\\'" 'python-mode)
        ;; (cons "/BUILD\\'" 'google3-build-mode)
        (cons "\\.proto\\'" 'protobuf-mode)
        (cons "\\.protodevel\\'" 'protobuf-mode)
        ;; Mendel files
        (cons "mendel/.*\\.gcl\\'" 'mendel-mode)
        (cons "gws.*\\.gcl\\'" 'mendel-mode)
        ;; Various Borg-related files
        (cons "\\.bcl\\'" 'borg-mode)
        (cons "\\.borg\\'" 'borg-mode)
        (cons "\\.btcfg\\'" 'borg-mode)
        (cons "\\.gcl\\'" 'borg-mode)
        (cons "\\.gclx\\'" 'borg-mode)
        (cons "\\.mcl\\'" 'borg-mode)
        (cons "\\.mw\\'" 'borg-mode)
        ;; Dremel query files
        (cons "\\.dremel\\'" 'sql-mode))
       auto-mode-alist))

;; Hacked together BUILD mode
(require 'python)
;; All rules are at http://go/be.
(defvar google3-build-mode-rules
  '(
;; Rules to Compile Code or Run Tests
    cc_binary css_binary go_binary haskell_binary java_binary
    js_binary js_module_binary py_binary sh_binary szl_binary
    borgcfg_library cc_library cc_public_library css_library go_library
    haskell_library java_library js_library proto_library proto_library_shell
    py_library sh_library szl_library qt_library borgcfg_test cc_test go_test
    haskell_test java_test js_test py_test saw_test sh_test szl_test test_suite
    cc_plugin android_binary android_library android_test as_binary as_library
    as_resource as_swc as_test java_import jsunit_test py_appengine_binary
    scala_library szl_library go_appengine_binary go_appengine_library
    java_proto_library java_lite_proto_library java_mutable_proto_library
    py_proto_library py_clif_cc pyclif_proto_library haskell_proto_library
    dart_proto_library dart_library dart_pub_library dart_pub_serve
    dart_pub_web_binary dart_vm_binary dart_vm_snapshot dart_vm_test
    dart_web_binary jspb_proto_library objc_proto_library pytype_binary
    pytype_library
;; Rules to Generate Code and Data
    cc_embed_data genantlr gendeb gendpl GenJs gengxp genjh genjsp genlex
    genmsgcat genproto genprotohdf genprotojs genrpm genrule gentpl gentplvars
    genyacc go_wrap_cc gwt_application gwt_host gwt_module gwt_test java_genmsg
    java_plugin java_wrap_cc js_deps pkgfilegroup py_extension py_wrap_cc
    Fileset rosy_generator android_idl android_resources genquery genrosy
    genrule jslayout_template translations web_test
;; "Make" Variables
    vardef deflocal varref MarkAsFilenameComponent
;; Other Stuff
    cc_fake_binary Description distribs exports_files glob filegroup include
    licenses load package package_group pinto_library pinto_library_mod
    pinto_module pinto_module_set set_inc_symlink subinclude
    PYTHON-PREPROCESSING-REQUIRED VERSION cc_inc_library action_listener
    extra_action genmpm pkg_library pkg_runfiles pkg_symlink
    )
  "All BUILD rules.")

;; These were just lumped together and uniq-ed.
;; To make it nicer, attributes should be highlighted only in correct rules.
(defvar google3-build-mode-attributes
  '(
    abi abi_deps abi_srcs access actions alwayslink antlr_opts antlr_version
    arch args artifacts attr base_inherits bootstrap_file borgcfgopts browsers
    build_for_appengine builder buildpar cc cc_api_compatibility cc_api_version
    cc_lib cc_plugin cc_stubby_versions classpath_resources closure_compliant
    cmd compatible_with compile compiled compiler compiler_class compiler_flags
    compiler_jvm_flags compiler_opts compress config_files configs constraints
    copts cpp_flags create_executable dart_api_version data debug_key
    default_hdrs_check default_strict_java_deps default_visibility defines
    defines_main defs depend deploy_env deploy_manifest_lines deprecation deps
    descriptor dexopts distrib_methods distribs distro documentation_flags
    dplcopts dump_codex dynamic embedopts enable_api_override
    encrypted encrypted_data_acl encrypted_file_umask
    entities entries entry_points env executable exported_deps exports
    expression external_libraries externs externs_list extra_actions
    extra_control_fields extra_inherits extra_inputs extra_module_contents
    extra_outputs extra_requires extractor file_umask filegroups flaky
    flash_player_version flash_version flatten flex_version gc_goopts
    gc_linkopts gccgo_goopts gccgo_linkopts ghcopts go_api_version goopts group
    gwtxml has_services heuristic_label_expansion hdrs hdrs_check headers
    headers_check html_body i18nwarn implementation implements include_libraries
    includes inherit_default_requires input instrumentation
    internal_bootstrap_hack jars java_api_version java_memory_limit
    java_stubby_noloas java_stubby_version javacopts js_api_version js_codegen
    js_libs jvm_flags lang legacy legacy_override_module library licences
    license_types licenses link_name link_target linkopts linkshared linkstamp
    linkstatic load_externs local locale_srcs locales logtype long_description
    main main_class main_is malloc max_blaze_version merge_deps message
    min_blaze_version mnemonics mod_name mode mods module_id module_target msgs
    mtasc_version name namespace neverlink nocopts obsolete opts out
    out_templates output_languages output_licenses output_sar output_to_bindir
    outs outs_template owner package_dir package_name package_path
    package_prefix packager packages paropts parser_config path plugins
    post_activate_commands postinst postrm pre_deactivate_commands prefix
    preinst preprocessor prerm priority processor_class
    proguard_generate_mapping proguard_spec pubs py_api_version release
    rename_to replica_cleanup_policy resaw_table resource resource_xml resources
    restricted_to rpm_builder rpm_name runtime saw_flags scalacopts schemas scope
    section servlets shard_count shell_class short_description size sourcetype
    spec src srcjar srcs stamp strict strict_java_deps strip strip_prefix
    stylesheet stylesheettype suites szlopts tags target target_platforms
    tc_project test test_class test_timeout testonly tests timeout tools
    trace_function triggers type urgency uriroot use_project_xmb use_testrunner
    user_copts version visibility weak_deps wrapper xmb
    )
  "All possible attributes in BUILD files.
No association with rules for now.")
(defvar google3-build-mode-fileset
  '(
    entries FilesetEntry srcdir files destdir
   )
  "Fileset attributes should only be available once fileset is subincluded.")
(defun google3-build-mode-setup-imenu ()
  "Set up imenu to index target names in BUILD files."
  (when (eq major-mode 'google3-build-mode)
    (setq imenu-generic-expression '((nil "name = \"\\(.*\\)\"" 1)))
    (setq imenu-create-index-function 'imenu-default-create-index-function)))
(define-derived-mode google3-build-mode python-mode
  "build"
  "Major mode for editing BUILD files"
  ;; The default C-c C-c, `python-send-buffer', is pointless in BUILD files.
  (define-key google3-build-mode-map "\C-c\C-c" 'comment-region)
  ;; Set indentation offset to 4 for BUILD files.  This is mandated by
  ;; go/build-style.
  (set (make-local-variable 'python-indent-offset) 4)
  (font-lock-add-keywords
   'google3-build-mode
   `(
     (,(google3-build-mode-regexp google3-build-mode-rules)
      1 font-lock-type-face)
     (,(google3-build-mode-regexp google3-build-mode-attributes)
      1 'font-lock-keyword-face)
     (,(google3-build-mode-regexp google3-build-mode-fileset)
      1 'font-lock-function-name-face)))
  (google3-build-mode-setup-imenu))

(setenv "GOPATH" "/usr/local/google/home/gmbuell/go")
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/google/home/gmbuell/go/bin"))
(setq exec-path (append exec-path (list (expand-file-name "/usr/local/google/home/gmbuell/go/bin"))))

;; Golang
(use-package company-go
  :after (company go-mode)
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  )

(use-package go-flycheck :load-path "/usr/local/google/home/gmbuell/go/src/github.com/dougm/goflymake")

(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package godoctor
  :ensure t)

(use-package flycheck-golangci-lint
  :ensure t
  :init
  :hook (go-mode . flycheck-golangci-lint-setup))


;; Shell setup
(use-package eterm-256color
  :ensure t
  :init (add-hook 'term-mode-hook #'eterm-256color-mode))

(use-package shell-pop
  :defer t
  :custom
  ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
  (shell-pop-universal-key "C-t")

  ;; Percentage for shell-buffer window size.
  (shell-pop-window-size 30)

  ;; Position of the popped buffer: top, bottom, left, right, full.
  (shell-pop-window-position "bottom")

  ;; Please use an awesome shell.
  (shell-pop-term-shell "/bin/bash"))

;; https://github.com/szermatt/emacs-bash-completion
;; (use-package bash-completion
;;   :disabled
;;   :ensure t
;;   :init (bash-completion-setup))

;; (use-package term-projectile
;;   :disabled
;;   :ensure t)

;; (use-package multi-term
;;   :ensure t
;;   :bind (("C-c t" . multi-term-dedicated-toggle))
;;   :init (setq multi-term-dedicated-select-after-open-p t
;;               ;;multi-term-program "/usr/local/bin/zsh"
;;               multi-term-program "/usr/bin/zsh"
;;               multi-term-program-switches "--login"
;;               ;;multi-term-dedicated-skip-other-window-p t
;;               ))

(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)

(use-package xterm-color
  :ensure t
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun get-last-term-url ()
  "Returns the last url in a terminal buffer."
  (interactive)
  (let ((result (caar (last (s-match-strings-all "https?://[^[:space:]\n.]+" (buffer-whole-string (car (get-buffers-matching-mode 'term-mode))))))))
    (kill-new result)
    result))
(bind-key "C-c l" 'get-last-term-url)

(use-package origami
  :ensure t
  :bind (("C-c <tab>" . origami-forward-toggle-node))
  :config (global-origami-mode))
(use-package hydra
  :ensure t
  :bind (("C-c r" . hydra-pause-resume)))
(defhydra hydra-origami (:color red)
  "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("a" origami-toggle-all-nodes))

;; Show hydras overlyaed in the middle of the frame
(use-package hydra-posframe
  ;; Only enable if we are running graphical mode.
  :if window-system
  :quelpa (hydra-posframe :fetcher git :url
                          "https://github.com/Ladicle/hydra-posframe.git")
  :hook (after-init . hydra-posframe-mode)
  :custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra
  :ensure t)

(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("<SPC>" avy-goto-word-1 "char")
  ("q" nil "quit"))

(use-package breadcrumb
  :quelpa (breadcrumb :fetcher git :url
                      "https://github.com/pheaver/breadcrumb.git"))
(defhydra hydra-breadcrumb
  (:exit t)
  "
Breadcrumb bookmarks:
  _<up>_:   prev   _S-<up>_:   local prev
  _<down>_: next   _S-<down>_: local next
  _s_: set  _c_: clear  _l_: list  _q_: quit
"
  ("<down>" bc-next nil :exit nil)
  ("<up>" bc-previous nil :exit nil)
  ("S-<down>" bc-local-next nil :exit nil)
  ("S-<up>" bc-local-previous nil :exit nil)
  ("l" bc-list nil)
  ("s" bc-set nil)
  ("c" bc-clear nil)
  ("q" nil nil))

;; One more code navigation method because kythe/clangd are unreliable.
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))


(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)
(setq set-mark-command-repeat-pop t)

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(use-package beginend
  :ensure t
  :init
  (beginend-global-mode))

;; Disable golden ration for now because it's a bit janky.
;; (use-package golden-ratio
;;   :ensure t
;;   :init
;;   (golden-ratio-mode 1)
;;   ;; important for golden-ratio to better work
;;   (setq window-combination-resize t)
;;   ;;:config
;;   ;;(setq golden-ratio-auto-scale t)
;;   )

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package help-fns+
  :bind ("C-h M-k" . describe-keymap)) ; For autoloading.

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(prefer-coding-system 'utf-8)

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(require 'server)
(unless (server-running-p) (server-start))
