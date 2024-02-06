;; Don't load site default
(setq inhibit-default-init t)
;; Also need to start emacs with --no-site-file because that happens before this
;; init.

(setq comp-deferred-compilation t)
(setq package-native-compile t)

(setq gc-cons-threshold 100000000)
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package))

;; Always load newest byte code
(setq load-prefer-newer t)

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   ;;  Delete residual old versions.
;;   (setq auto-package-update-delete-old-versions t)
;;   ;; Update every week.
;;   (setq auto-package-update-interval 7)
;;   ;; Do not bother me when updates have taken place.
;;   (setq auto-package-update-hide-results t)
;;   ;;Update installed packages at startup if there is an update pending.
;;   :init (auto-package-update-maybe))

;; Add quelpa for an additional way of adding packages
(use-package quelpa
  :ensure t
  :config
  ;;(setq quelpa-upgrade-p t)
  (setq quelpa-upgrade-interval 7)
  :init (quelpa-upgrade-all-maybe))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(git-gutter:handled-backends '(git hg bzr svn))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(ace-window aio auto-yasnippet bash-completion bazel beginend breadcrumb cape clipetty copilot corfu-terminal deft diminish discover-my-major dogears doom-themes dumb-jump embark-consult expand-region find-file-in-project flymake-golangci fold-this git-gutter go-mode highlight-symbol iflipb link-hint magit-todos marginalia markdown-mode modern-cpp-font-lock mosey multifiles multiple-cursors nov orderless origami phi-search pretty-hydra projectile protobuf-mode quelpa-use-package rainbow-delimiters region-bindings-mode shelldon smart-mode-line smartparens smartscan vertico walkman which-key xterm-color yaml-mode yasnippet-snippets))
 '(sp-override-key-bindings
   '(("C-<right>" . sp-slurp-hybrid-sexp)
     ("C-<left>" . sp-dedent-adjust-sexp)))
 '(warning-suppress-log-types '((comp))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flymake-error ((t (:underline (:color "#e74c3c" :style wave :position wave))))))

;; Disable graphical garbage
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
   (scroll-bar-mode -1))
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------
;; Auto refresh
;; Global auto revert might hang things?
(global-auto-revert-mode 1)
;; (global-auto-revert-mode -1)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
(setq auto-revert-check-vc-info t)
;; Slow down auto revert.
(setq auto-revert-interval 10)

(use-package dired
  :commands dired
  :config
  ;; Make dired "guess" target directory for some operations, like copy to
  ;; directory visited in other split buffer.
  (setq dired-dwim-target t)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  ;; Make dired re-use buffers when pushing RET or ^
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; Simplify listing
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package dired-x
  :after (dired)
  :commands dired-jump
  :config
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
              tab-width 2)
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

;; C++ style
;; Not sure if we want google-c-style enabled or not. It's old emacs.
;; (use-package google-c-style
;;   :ensure t
;;   :init (progn
;;           (add-hook 'c-mode-common-hook 'google-set-c-style)
;;           (add-hook 'c-mode-common-hook 'google-make-newline-indent)))
(use-package cc-mode
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'subword-mode))

;; Headers are c++, not c
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Stop indenting in namespaces
(c-set-offset 'innamespace 0)

;; Get syntax hilighting for modern c++
(use-package modern-cpp-font-lock
  :ensure t
  :init (modern-c++-font-lock-global-mode t))


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

;; Now onto packages
;; ====================================================

;; Load hydra early since I've got defhydras scattered throughout.
(use-package hydra
  :ensure t
  :demand t
  :bind (("C-c r" . hydra-pause-resume)))

;; (setq whitespace-style '(face trailing lines-tail tabs)
;;       whitespace-line-column 80
;;       save-place-file (concat user-emacs-directory "places"))

;; Enable virtual buffers
(recentf-mode 1)
(setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                        "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                        ".*png$" ".*cache$"))

;; For fuzzy searching.
;; (use-package flx
;;   :ensure t)

;; Look at these release notes to see other cool ivy stuff
;; https://oremacs.com/2016/04/26/ivy-0.8.0/
;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind (("C-c C-r" . ivy-resume)
;;          :map ivy-minibuffer-map
;;          ("C-m" . ivy-alt-done))
;;   :config
;;   (setq ivy-extra-directories nil)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   ;; FYI, M-r will disable fuzzy matching for a single search.
;;   ;; I've used ivy--regex-fuzzy in the past but it's a disaster too many places.
;;   (setq ivy-re-builders-alist
;;         '((counsel-M-x . ivy--regex-fuzzy)
;;           (counsel-find-file . ivy--regex-fuzzy)
;;           (ivy-switch-buffer . ivy--regex-fuzzy)
;;           (counsel-git . ivy--regex-fuzzy)
;;           (t . ivy--regex-plus)))
;;   ;; Get rid of initial ^ to make everything fuzzy.
;;   (setq ivy-initial-inputs-alist nil)
;;   ;; This solves the long standing issue of e.g. creating a file or a directory
;;   ;; foo when a file foobar already exists. Previously, the only solution was to
;;   ;; use C-M-j. It's still available, but now you can also select your input with
;;   ;; C-p and press RET.
;;   (setq ivy-use-selectable-prompt t)
;;   :init
;;   (ivy-mode 1))

;; (use-package ivy-hydra
;;   :after (ivy hydra)
;;   :ensure t)

;; Consider also occur mode
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L477-L483
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L938-L947
;; https://oremacs.com/2015/01/26/occur-dwim/
;; https://github.com/abo-abo/hydra/wiki/Emacs

;; (use-package swiper
;;   :ensure t
;;   ;; :bind (("C-s" . counsel-grep-or-swiper))
;;   ;; :bind (("C-s" . swiper-isearch))
;;   )
;; (use-package counsel
;;   :ensure t
;;   :diminish counsel-mode
;;   :bind (("M-i" . counsel-semantic-or-imenu)
;;          ;; ("C-c h" . counsel-git)
;;          :map minibuffer-local-map
;;          ("C-r" . counsel-minibuffer-history))
;;   :init
;;   (counsel-mode 1))

;; Lets try using ripgrep (rg) for everything
;; (setq counsel-git-cmd "rg --files -0")
;; (setq counsel-rg-base-command
;;       "rg -i -M 120 --no-heading --line-number --color never %s .")
;; (setq counsel-grep-base-command
;;       "rg -i -M 120 --no-heading --line-number --color never %s .")
;; I've also seen this command recommended. Not sure if it works.
;; (setq counsel-grep-base-command
;;     "rg -i -M 120 --no-heading --line-number --color never %s %s")

;; Git integration
;; -------------------------------------------------------------------
;; Shows git (and hg!) diff information in the gutter.
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t)
  :config
  (custom-set-variables
   '(git-gutter:handled-backends '(git hg bzr svn))))

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
  (setq auto-revert-check-vc-info t)
  (setq vc-follow-symlinks t))

(defun my/google3-early-exit (orig-fun &rest args)
  (if (string-prefix-p "/google/src/cloud/" (buffer-file-name))
      (progn (message "my/google3-early-exit overrode.") nil)
    (apply orig-fun args)))

(advice-add 'magit-toplevel :around #'my/google3-early-exit)
(advice-add 'magit-inside-worktree-p :around #'my/google3-early-exit)

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

(use-package magit-todos
  :ensure t
  :init
  (magit-todos-mode))

;; smerge-mode instead of ediff
(use-package smerge-mode
  :requires hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;; Hg/Fig setup
;; Use chg instead of hg since it's faster.
(setq vc-hg-program "chg")

;; Tell emacs not to try and do anything fancy with version control besides git
;; or hg.
(setq vc-handled-backends '(Git Hg))
;; (use-package vc-defer
;;   :quelpa (vc-defer :fetcher git :url
;;                          "https://github.com/google/vc-defer")
;;   :config
;;   (add-to-list 'vc-defer-backends 'Git)
;;   (add-to-list 'vc-defer-backends 'Hgcmd)
;;   ;; Maybe should have?
;;   ;; (add-to-list 'vc-defer-backends 'Hg)
;;   (vc-defer-mode)
;;   :init
;;   (advice-add 'vc-root-dir :around 'vc-defer--deduce-fileset-around))

;; Replace all the search keys with regex versions.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

(defalias 'auto-tail-revert-mode 'tail-mode)

(use-package saveplace
  :ensure t
  :config
  (setq select-enable-clipboard t
        select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  :init
  (save-place-mode +1))

;; From http://steckerhalter.co.vu/steckemacs.html
;; Make isearch-forward put the cursor at the start of the search, not the end.
(defun my-isearch-goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-isearch-goto-match-beginning)

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-on-navigation-p t)
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

;; (add-hook 'prog-mode (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace t)))
;; Actually, just always do this
(add-hook 'before-save-hook 'delete-trailing-whitespace t)

(set-default 'imenu-auto-rescan t)
;; Teach imenu to recognize TEST_F
(add-hook 'c++-mode
          (lambda ()
            (when (s-ends-with? "_test.cc" buffer-file-name)
              (setq-local
               imenu-generic-expression
               '(("Class" "^\\(template[        ]*<[^>]+>[      ]*\\)?\\(class\\|struct\\)[     ]+\\([[:alnum:]_]+\\(<[^>]+>\\)?\\)\\([         \n]\\|\\\\\n\\)*[:{]" 3)
                 ("Test" "^ *TEST\\(?:_F\\)?([^,]+,\n? *\\(.+\\)) {$" 1))))))

;; This seems to massively slow down opening files and errors frequently.
;; (use-package which-func
;;   :diminish which-func-mode
;;   :init
;;   (which-function-mode)
;;   (setq-default header-line-format
;;                 '((which-func-mode ("" which-func-format " ")))))

(use-package markdown-mode
  :ensure t)

;; Deft is my preferred note-taking setup. See:
;; http://jblevins.org/projects/deft/
(use-package deft
  :ensure t
  :config
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

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

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
;; Also consider using goal column?
(use-package mosey
  :ensure t
  :init
  (defmosey '(beginning-of-line
              back-to-indentation
              sp-backward-sexp  ;; Moves across lines
              sp-end-of-sexp    ;; Might be bad
              sp-forward-sexp   ;; Moves across lines
              mosey-goto-end-of-code
              mosey-goto-beginning-of-comment-text
              end-of-line)
    :prefix "c")
  :bind* (("C-a" . mosey-backward-bounce)
          ("C-e" . mosey-forward-bounce)))

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
;; (use-package ujelly-theme
;;   :ensure t
;;   :init
;;   (load-theme 'ujelly t))

;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))

;; (use-package all-the-icons
;;   :ensure t)
;; (use-package neotree
;;   :ensure t)
;; (use-package powerline
;;   :ensure t)
;; (use-package airline-themes
;;   :ensure t)

;; Too many blue colors
;; (use-package jbeans-theme
;;   :ensure t
;;   :config
;;   (load-theme 'jbeans t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package solaire-mode
;;   :ensure t
;;   :init
;;   (solaire-global-mode +1))

(defadvice custom-theme-load-confirm (around no-query-safe-theme activate)
  t)

;; (use-package base16-theme
;;   ;; :if window-system
;;   :ensure t
;;   :init
;;   (setq base16-theme-256-color-source 'colors)
;;   (load-theme 'base16-monokai t))

(when (display-graphic-p)
  (set-frame-font "Inconsolata 12" t t))

(use-package re-builder
  :ensure t
  :config (setq reb-re-syntax 'string))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/lisp/snippets")
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure yasnippet-snippets
  :requires (yasnippet))
;; (use-package yasnippet
;;   :ensure t
;;   :diminish yas-minor-mode
;;   :config
;;   (setq yas-triggers-in-field t)
;;   :init
;;   (yas-global-mode 1))

;; persistent abbreviation file
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
;; Disable abbrev-mode
(setq-default abbrev-mode -1)
;; Make sure it stays disabled
(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode -1)))

(add-hook 'prog-mode-hook 'subword-mode)

(use-package org
  :ensure t
  :init
  (setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
  :config
  ;; (bind-key "C-c SPC" 'ace-jump-mode org-mode-map)
  ;; Add shortcut to recalculate table
  ;; (bind-key "M-r" '(lambda () (interactive)(org-table-recalculate t)) org-mode-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (comint . t)
     (sqlite . t)
     (emacs-lisp . t)
     ;;(dremel . t)
     )))

(use-package walkman
  :ensure t
  :config
  (setq walkman-keep-headers t)
  :bind (:map org-mode-map
              ("C-c w" . walkman-transient)
              ("C-c e" . walkman-at-point)
              ))


;; M-n and M-p move between symbols
;; M-' to replace all symbols in the buffer matching the one under point
;; C-u M-' to replace symbols in your current defun only (as used by narrow-to-defun.)
(use-package smartscan
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; (use-package icomplete
;;   :init
;;   (fido-vertical-mode +1)
;;   :config
;;   ;; I wish this worked but it doesn't.
;;   ;; (setq icomplete-in-buffer t)
;;   (setq icomplete-tidy-shadowed-file-names t)
;;   (setq icomplete-matches-format nil))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
      '((consult-imenu buffer indexed)))
  :bind (("M-i" . consult-imenu)))

;; Completion
;; headlong might be useful for bookmark jumping

;; Could also try out restricto as an alternative
;; https://github.com/oantolin/restricto
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(substring flex basic))
  (orderless-smart-case t))
  ;; :config
  ;; Allow space in minibuffer completions
  ;; (let ((map minibuffer-local-completion-map))
  ;;   (define-key map (kbd "SPC") nil)))

(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun first-initialism (pattern index _total)
  (if (= index 0) 'orderless-initialism))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

;; (setq orderless-matching-styles '(orderless-regexp)
;;       orderless-style-dispatchers '(first-initialism
;;                                     flex-if-twiddle
;;                                     without-if-bang))

;; Try savehist for minibuffer history
(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

(use-package embark
  :ensure t
  ;; :bind.
  ;; (("C-." . embark-act)         ;; pick some comfortable binding
  ;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
  ;;  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t)

(use-package embark-consult
  :ensure t)

(defvar consult--source-dogears
  (list :name     "Dogears"
        :narrow   ?d
        :category 'dogears
        :items    (lambda ()
                    (mapcar
                     (lambda (place)
                       (propertize (dogears--format-record place)
                                   'consult--candidate place))
                     dogears-list))
        :action   (lambda (cand)
                    (dogears-go (get-text-property 0 'consult--candidate cand)))))

(defun consult-dogears ()
  (interactive)
  (consult--multi '(consult--source-dogears)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1)
         ;;("C-c C-j" . avy-resume)
         )
  :init
  (avy-setup-default)
  (setq avy-style 'at)
  (setq avy-background t)
  (setq avy-keys (number-sequence ?a ?z)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Project managment
;; Also look into bufler for this
;; https://github.com/alphapapa/bufler.el#compared-to-ibuffer
(use-package projectile
  :ensure t
  :bind* (("C-c h" . projectile-find-file)
         ("C-x p b" . projectile-switch-to-buffer)
         ("C-x p p" . projectile-switch-project))
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-git-command "fd . -t f -0")
  (setq projectile-generic-command "fd . -t f -0")
  (setq projectile-hg-command "fd . -t f -0")
  (setq projectile-globally-ignored-buffers spacemacs-useless-buffers-regexp)
  ;; (setq projectile-completion-system 'ivy)
  ;; Maybe enable cache for local file system
  ;; (setq projectile-file-exists-local-cache-expire (* 5 60))
  :init
  (projectile-global-mode))
(use-package find-file-in-project
  :ensure t
  :commands find-file-in-project)
;; (use-package counsel-projectile
;;   :after (counsel projectile)
;;   :ensure t
;;   :init (counsel-projectile-mode)
;;   :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package f
  :ensure t)

(defun refresh-projectile ()
  (interactive)
  "Reset projectile known projects to citc and git5 clients."
  (projectile-clear-known-projects)
  (mapc #'projectile-add-known-project
        (f-glob "~/fig/*/google3"))
  (mapc #'projectile-add-known-project
       (seq-remove (apply-partially 's-contains? "fig") (f-glob "/google/src/cloud/gmbuell/*/google3")))
  (projectile-save-known-projects))

(projectile-register-project-type 'google3 '("WORKSPACE"))

(add-hook 'after-init-hook 'refresh-projectile)

;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; ;; Allow company completions by selecting the completion number
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


(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; 10/26/2023 Disable corfu-auto because copilot is so much better
  ;;(corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  ;; (corfu-quit-no-match 'separator)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package corfu-indexed
  :requires (corfu)
  :init
  (corfu-indexed-mode 1))

(quelpa '(corfu-terminal
          :fetcher git
          :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(defun rk/copilot-complete-if-active (next-func n)
  (let ((completed (when copilot-mode (copilot-accept-completion))))
    (unless completed (funcall next-func n))))

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :init
  (add-hook 'prog-mode-hook 'copilot-mode)
  :config
  ;; keybindings that are active when copilot shows completions
  (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)

  ;; global keybindings
  (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)
  ;; 10/26/2023 Don't rebind dabbrev-expand (which should be C-/) so I stil have other completion mechanisms available.
  ;; (global-set-key [remap dabbrev-expand] #'rk/copilot-complete-or-accept)
  ;; Do copilot-quit when pressing C-g
  (advice-add 'keyboard-quit :before #'rk/copilot-quit)

  ;; complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  (advice-add 'right-char :around #'rk/copilot-complete-if-active)
  (advice-add 'indent-for-tab-command :around #'rk/copilot-complete-if-active))

;; (use-package company
;;   :ensure t
;;   ;;:bind ([remap indent-for-tab-command] . company-indent-or-complete-common)
;;   :bind* (:map prog-mode-map
;;                ("TAB" . company-indent-or-complete-common))
;;   :init
;;   (global-company-mode)
;;   (setq company-tooltip-limit 9)                      ; bigger popup window
;;   (setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
;;   (setq company-minimum-prefix-length 3)
;;   (setq company-echo-delay (if (display-graphic-p) nil 0))  ; Maybe this should just be always 0?
;;   (setq company-require-match nil)                     ;  Allow exiting the completion
;;   (setq company-show-numbers t)
;;   (setq company-backends '((company-capf :with company-dabbrev-code))) ;; company-dabbrev-code
;;   (let ((map company-active-map))
;;     (mapc
;;      (lambda (x)
;;        (define-key map (format "%d" x) 'ora-company-number))
;;      (number-sequence 0 9))
;;     (define-key map " " (lambda ()
;;                           (interactive)
;;                           (company-abort)
;;                           (self-insert-command 1)))
;;     (define-key map (kbd "<return>") nil))
;;   ;; If completing on . or -> is too aggressive:
;;   (setq company-begin-commands '(self-insert-command))
;;   ;; Remove company-clang since we don't use it.
;;   )

;; (use-package company-try-hard
;;   :ensure t
;;   :bind* (("C-z" . company-try-hard)
;;           :map company-active-map
;;           ("C-z" . company-try-hard)))

;; 10/26/2023 Disable complete because copilot is so much better.
;; (setq tab-always-indent 'complete)
;; (defun gmbuell-indent-or-complete-common (arg)
;;   "Indent the current line or region, or complete the common part."
;;   (interactive "P")
;;   (cond
;;    ((use-region-p)
;;     (indent-region (region-beginning) (region-end)))
;;    ((memq indent-line-function
;;           '(indent-relative indent-relative-maybe))
;;     (completion-at-point))
;;    ((let ((old-point (point))
;;           (old-tick (buffer-chars-modified-tick))
;;           (tab-always-indent t))
;;       (indent-for-tab-command arg)
;;       (when (and (eq old-point (point))
;;                  (eq old-tick (buffer-chars-modified-tick)))
;;         (completion-at-point))))))

;; (bind-key "TAB" 'gmbuell-indent-or-complete-common c++-mode-map)

;; Doesn't work in terminal mode. For documentation popups on idle.
;; (use-package pos-tip
;;   :ensure t)
;; (use-package company-quickhelp
;;   :after (pos-tip)
;;   :ensure t
;;   :init (add-hook 'after-init-hook 'company-quickhelp-mode))

;; This might be crap. The backends might already give good completion ordering.
;; (use-package company-statistics
;;   :after (company)
;;   :ensure t
;;   :init
;;   (company-statistics-mode))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :init
  (add-hook 'find-file-hook 'flymake-find-file-hook))

(use-package eglot
  :requires (yasnippet) ;; company
  :ensure t
  :init
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'protobuf-mode-hook 'eglot-ensure)
  ;; Stop this flymake workaround because I think everything is fine with eglot and flymake now.
  ;;(setq eglot-stay-out-of '(flymake))
  ;;(add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)))
  :config
  (add-to-list
   'eglot-server-programs
   '((python-mode)
     . ("/google/bin/releases/editor-devtools/ciderlsp" "--noforward_sync_responses" "--request_options=enable_placeholders" "--tooltag=emacs-eglot")))
  ;; kythe is deprecated but it works on unsubmitted code.
  ;; But no autocomplete
  (add-to-list
   'eglot-server-programs
   ;; '((c++-mode c-mode java-mode python-mode protobuf-mode)
   '((protobuf-mode mendel-mode borg-mode google3-build-mode)
     . ("/google/bin/releases/grok/tools/kythe_languageserver" "--google3")))
  ;; "-index-file=/usr/local/google/home/gmbuell/index.idx"
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "-cross-file-rename")))
  ;;(add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("/google/bin/releases/grok/tools/kythe_languageserver" "--google3")))
  )

;; Tell eglot about go modules
(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)
;; eldoc is very noisy
;; Going to see if eldoc is better now
;;(global-eldoc-mode -1)
;;(add-hook 'prog-mode-hook
;;          (lambda ()
;;            (eldoc-mode -1)))

;; Also use dabbrev for completion
;; (defun add-cape-dabbrev ()
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
;;   )
(use-package cape
  :ensure t
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;;(add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  ;;(add-hook 'c++-mode-hook #'add-cape-dabbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; (use-package ivy-xref
;;   :after (ivy)
;;   :ensure t
;;   :init
;;   ;; xref initialization is different in Emacs 27 - there are two different
;;   ;; variables which can be set rather than just one
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;;   ;; commands other than xref-find-definitions (e.g. project-find-regexp)
;;   ;; as well
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(require 'nadvice)
(defun do-nothing (orig-fun &rest args)
  t)

(use-package region-bindings-mode
  :ensure t)
(require 'region-bindings-mode)
(region-bindings-mode-enable)

;; It may be possible to use avy to set marks for multiple cursors.
;; I.e. avy-push-mark
;; Also via swiper https://oremacs.com/2015/10/14/swiper-mc/
(use-package multiple-cursors
  :ensure t
  :demand t
  ;; Alternative bindings because hterm sucks at passing through keys
  :bind* (
          ("M-m e" . mc/edit-lines)
          ("M-m n" . mc/mark-next-like-this)
          ("M-m p" . mc/mark-previous-like-this)
          ("M-m a" . mc/mark-all-symbols-like-this-in-defun)
          ("M-m A" . mc/mark-all-symbols-like-this)
          ("M-m h" . mc-hide-unmatched-lines-mode)
          :map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("m" . mc/mark-more-like-this-extended)
              ("f" . mc/skip-to-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("h" . mc-hide-unmatched-lines-mode)
          ))

(use-package multifiles
  :ensure t
  :demand t
  :bind (:map region-bindings-mode-map
              ("M" . mf/mirror-region-in-multifile)))

(use-package fold-this
  :ensure t
  :demand t
  :bind (:map region-bindings-mode-map
              ("F" . fold-active-region-all)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         :map region-bindings-mode-map
         ("u" . er/contract-region)))

;; phi search works with multiple cursors But has weird behavior around trying
;; to move while in a search. Fortunately, multiple cursors mode seems to use it
;; automatically even if not set as default.
(use-package phi-search
  :ensure t
  :demand t)
(use-package phi-replace
  ;; Doesn't have it's own package.
  :requires (phi-search)
  :demand t)

;; Consider also placeholder
;; https://github.com/oantolin/placeholder
(use-package auto-yasnippet
  :requires (yasnippet)
  :ensure t
  :bind (("M-w" . aya-create)
         ("M-W" . aya-expand)))

;; Doesn't currently do anything because I've remapped M-w which is usually
;; kill-ring-save.
;; (use-package easy-kill :ensure t :config (global-set-key
;; [remap kill-ring-save] 'easy-kill))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Protocol buffer support
(use-package protobuf-mode
  :ensure t
  :bind* (:map protobuf-mode-map
               ("TAB" . indent-for-tab-command)))

(if (file-exists-p "/usr/share/emacs/site-lisp/emacs-google-config")
    (progn
      (use-package google-borg-helpers :load-path "/usr/share/emacs/site-lisp/emacs-google-config/third_party/elisp/google_borg_helpers/")
      ;; Needed by borg-mode
      (use-package aio
        :ensure t)
      (use-package borg-mode :load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/")))


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

(setq next-error-message-highlight t)

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

(defun google3-build-mode-regexp (symbols)
  "Return an optimized regular expression matching the given SYMBOLS list.
Similar to `regexp-opt' with `'words', but using symbol
delimiters instead of word delimiters."
  (concat "\\_<" (regexp-opt (mapcar 'symbol-name symbols) t) "\\_>"))

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

(use-package bazel
  :ensure t
  :init
  (setq bazel-command '("bazel"))
  :bind (("C-c C-c" . bazel-build)))

(setq auto-mode-alist
      (nconc
       (list
        (cons "\\.pyplan\\'" 'python-mode)
        ;; MPM package definition files are Python syntax.
        (cons "/pkgdef\\'" 'python-mode)
        (cons "/BUILD\\'" 'google3-build-mode)
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

(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/go/bin"))
(setq exec-path (append exec-path (list (expand-file-name (concat (getenv "HOME") "/go/bin")))))

;; Golang
;; Install gopls lsp server
;; go install golang.org/x/tools/gopls@latest
;; Also need to do more setup to get this to work with bazel.
;; https://github.com/bazelbuild/rules_go/wiki/Editor-setup
;; (defun eglot-format-buffer-on-save ()
;;   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'eglot-ensure)
  ;;(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
  )

;; Install golangci-lint
;; curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.50.1
;; Disable golangci because I'm using bazel.
;; (use-package flymake-golangci
;;   :ensure t
;;   :init
;;   (add-hook 'go-mode-hook 'flymake-golangci-load))


;;(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;;(require 'go-flymake)

;; (use-package flymake-go-staticcheck
;;   :ensure t
;;   :init
;;   (add-hook 'go-mode-hook #'flymake-go-staticcheck-enable))

;; Shell setup
;; (use-package eterm-256color
;;   :ensure t
;;   :init (add-hook 'term-mode-hook #'eterm-256color-mode))

(setq enable-recursive-minibuffers t)
(use-package bash-completion
  :ensure t
  :init (bash-completion-setup))

;; https://github.com/Overdr0ne/shelldon
(use-package shelldon
  :ensure t
  :bind* (("C-t" . shelldon)
          ("M-t" . shelldon-output-history))
  :init
  (setq shell-command-prompt-show-cwd t)
  ;; (setq shell-command-switch "-ic")
  (setq shelldon-ansi-colors t)
  ;; xterm-color below might be better
  ;(add-hook 'shelldon-mode-hook 'ansi-color-for-comint-mode-on)
  ;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
                                        ;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (defun test-go ()
    (interactive)
    (shelldon-async-command "bazel test --test_output=all :all"))
  )

(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)

(use-package xterm-color
  :ensure t
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
  ;;(setq compilation-environment '("TERM=xterm-256color"))
  (setq compilation-environment '("TERM=xterm-24bits"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))


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
;; (bind-key "C-c l" 'get-last-term-url)
;; Instead try link-hint
(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package origami
  :ensure t
  :bind (("C-c TAB" . origami-forward-toggle-node))
  :config (global-origami-mode)
  :init
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
    ("a" origami-toggle-all-nodes)))

;; Show hydras overlayed in the middle of the frame
(use-package hydra-posframe
  ;; Only enable if we are running graphical mode.
  :if window-system
  :quelpa (hydra-posframe :fetcher git :url
                          "https://github.com/Ladicle/hydra-posframe.git")
  :hook (after-init . hydra-posframe-mode)
  :custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra
  :requires hydra
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
                      "https://github.com/pheaver/breadcrumb.git")
  :demand t)
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

(use-package dogears
  :quelpa (dogears :fetcher github :repo "alphapapa/dogears.el"
                   :files (:defaults (:exclude "helm-dogears.el")))
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :init
  (add-hook 'prog-mode-hook #'dogears-mode))
;; Another alternative is gumshoe
;; https://github.com/Overdr0ne/gumshoe

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
  ;;(setq dumb-jump-selector 'ivy)
  ;; (setq dumb-jump-force-searcher 'rg)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; (defhydra dumb-jump-hydra (:color blue :columns 3)
  ;;   "Dumb Jump"
  ;;   ("j" xref-find-definitions "Go")
  ;;   ("o" dumb-jump-go-other-window "Other window")
  ;;   ("e" dumb-jump-go-prefer-external "Go external")
  ;;   ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ;;   ("i" dumb-jump-go-prompt "Prompt")
  ;;   ("l" dumb-jump-quick-look "Quick look")
  ;;   ("b" xref-pop-marker-stack "Back"))
  )

(defun switch-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
;;(bind-key "M-o" 'switch-previous-buffer)
(use-package iflipb
  :ensure t
  :bind* (
          ;;("M-o" . iflipb-next-buffer)
          ;;("M-O" . iflipb-previous-buffer)
          ("C-x k" . iflipb-kill-buffer)))


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

(use-package beginend
  :ensure t
  :init
  (beginend-global-mode))

;; Make .sh files executable upon save.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package help-fns+
  :bind ("C-h M-k" . describe-keymap)) ; For autoloading.

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :config
;;   (setq graphviz-dot-indent-width 2))

;; (use-package company-graphviz-dot)

;; Calendars should start with Monday as the first day of the week.
(setq calendar-week-start-day 1)

;; Check out https://github.com/raxod502/prescient.el
;; And https://github.com/raxod502/selectrum

(require 'server)
(unless (server-running-p) (server-start))
(setenv "EDITOR" "TERM=xterm-24bits emacsclient -nw")


;; Future enhancements:
;; prism for highlighting modes without good syntax hilighting?
;; https://github.com/alphapapa/prism.el
