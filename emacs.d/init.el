;;; -*- lexical-binding: t -*-

;; Don't load site default
(setq inhibit-default-init t)
;; Also need to start emacs with --no-site-file because that happens before this
;; init.

(setopt package-native-compile t)
(setq native-comp-jit-compilation t)

(setq gc-cons-threshold 100000000)
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure nil)
(setq use-package-verbose t)

;; Always load newest byte code
(setq load-prefer-newer t)

(use-package bind-key
  :ensure t)

(use-package diminish :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(ace-window anzu apheleia auto-yasnippet bazel beginend cape
                casual-symbol-overlay clipetty consult-compile-multi consult-dir
                consult-eglot-embark corfu-prescient deft diminish diredfl
                dirvish discover-my-major disproject docker doom-themes eat
                expand-region fold-this git-gutter go-mode iflipb link-hint
                magit-todos marginalia markdown-mode mini-echo minuet
                modern-cpp-font-lock mosey multifiles nov ob-async pcmpl-args
                phi-search pretty-hydra projection-multi projection-multi-embark
                protobuf-mode rainbow-delimiters region-bindings-mode
                smartparens symbol-overlay-mc vertico-prescient vundo walkman
                wgrep yasnippet-snippets))
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

(defgroup my-minuet nil
  "Custom settings for minuet code completion."
  :group 'tools)

(defcustom my-minuet-enabled t
	"If minuet should be enabled for code completion"
	:type 'boolean
	:group 'my-minuet)

(defcustom my-minuet-model "starcoder2"
  "The model to use with minuet."
  :type 'string
  :group 'my-minuet)

(defcustom my-minuet-endpoint "http://192.168.1.4:8080/v1/completions"
  "The endpoint URL for minuet API calls."
  :type 'string
  :group 'my-minuet)

(defcustom my-minuet-api-key "TERM"
  "The API key for minuet."
  :type 'string
  :group 'my-minuet)

;; Define custom variables for clangd
(defcustom my-clangd-binary "clangd"
  "Path to the clangd binary."
  :type 'string
  :group 'development)

(defcustom my-clangd-args nil
  "List of arguments to pass to clangd."
  :type '(repeat string)
  :group 'development)

;; Mark variables as safe for file-local values
(put 'my-clangd-binary 'safe-local-variable #'stringp)
(put 'my-clangd-args 'safe-local-variable #'listp)

;; Custom function to create the clangd command
(defun my-clangd-command (interactive-p)
  "Return the command to run clangd based on current settings.
INTERACTIVE-P is non-nil if called interactively."
  (if my-clangd-args
      (cons my-clangd-binary my-clangd-args)
    (list my-clangd-binary)))

;; Example host-specific configuration
;; (setq my-clangd-binary "/path/to/custom/clangd")
;; (setq my-clangd-args '("--header-insertion=never" "--clang-tidy"))

;; Or with .dir-locals.el
;; ((c++-mode . ((my-clangd-binary . "/path/to/project/specific/clangd")
;;               (my-clangd-args . ("--header-insertion=never" "--clang-tidy"))))
;;  (c-mode . ((my-clangd-binary . "/path/to/project/specific/clangd")
;;             (my-clangd-args . ("--header-insertion=never" "--clang-tidy")))))

;; Load local config
(defun short-system-name ()
  "Return a shortened version of the system name.
For domain names with multiple components, returns the second-to-last component.
For simple hostnames, returns the full hostname."
  (let* ((full-name (system-name))
         (components (split-string full-name "\\.")))
    (if (> (length components) 1)
        ;; If multiple components, return the second-to-last one
        (nth (- (length components) 2) components)
      ;; Otherwise return the full name
      full-name)))
(let ((host-init-file (format "~/.emacs.d/init.%s.el" (short-system-name))))
  (when (file-exists-p host-init-file)
    (load-file host-init-file)))

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------
;; Auto refresh
(global-auto-revert-mode 1)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
(setq auto-revert-check-vc-info t)
;; Slow down auto revert.
(setq auto-revert-interval 10)

(use-package dired
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  ;; Make dired "guess" target directory for some operations, like copy to
  ;; directory visited in other split buffer.
  (setq dired-dwim-target t)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  ;; Make dired re-use buffers when pushing RET or ^
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  :after (dired)
  :commands dired-jump
  :hook
  (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

;; Additional syntax highlighting for dired
(use-package diredfl
  :ensure t
  :after (dirvish)
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :custom-face
  (diredfl-dir-name ((t (:bold t)))))

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar spacemacs-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")

(defvar spacemacs-useful-buffers-regexp '("\\*\\(scratch\\|eshell\\)\\*")
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
(setq-default tab-width 2
							go-ts-mode-indent-offset 2
							indent-tabs-mode nil)
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

(use-package cc-mode
  :ensure t
  :hook (c-mode-common . subword-mode))

;; Headers are c++, not c
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; Stop indenting in namespaces
(c-set-offset 'innamespace 0)

;; Get syntax hilighting for modern c++
(use-package modern-cpp-font-lock
  :ensure t
	:disabled t
  :config
  (modern-c++-font-lock-global-mode t))


;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Better cut/paste handling for ssh and tmux
(use-package clipetty
  :diminish clipetty-mode
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Single space between sentences
(setq-default sentence-end-double-space nil)

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

;; Show column number in mode line
(setq column-number-mode t)

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
;; Disable autosave files too
(setq auto-save-default nil)

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

;; Also region-bindings-mode early so I can use region-bindings-mode-map
(use-package region-bindings-mode
  :ensure t
  :demand t
  :init
  (require 'region-bindings-mode)
  (region-bindings-mode-enable))

(require 'project)
(bind-key "C-c h" 'project-find-file)

;; (setq whitespace-style '(face trailing lines-tail tabs)
;;       whitespace-line-column 80
;;       save-place-file (concat user-emacs-directory "places"))

;; Enable virtual buffers
(recentf-mode 1)
(setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                        "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                        ".*png$" ".*cache$"))

;; Consider also occur mode
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L477-L483
;; https://github.com/sawan/emacs-config/blob/013af97a03e5dd7493d50f35c9c4724fa7de88f5/emacs24.el#L938-L947
;; https://oremacs.com/2015/01/26/occur-dwim/
;; https://github.com/abo-abo/hydra/wiki/Emacs

;; Git integration
;; -------------------------------------------------------------------
;; Shows git (and hg!) diff information in the gutter.
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :custom
  (git-gutter:handled-backends '(git hg bzr svn))
  :config
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
  :bind ("C-c g" . unpackaged/magit-status)
  :custom
  (auto-revert-check-vc-info t)
  (vc-follow-symlinks t)
  (magit-display-buffer-function #'display-buffer)
  :init
  (defun unpackaged/magit-status ()
    "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
    (interactive)
    (let* ((buffer-file-path (when buffer-file-name
                               (file-relative-name buffer-file-name
                                                   (locate-dominating-file buffer-file-name ".git"))))
           (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
      (call-interactively #'magit-status)
      (delete-other-windows)
      (when buffer-file-path
        (goto-char (point-min))
        (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                         (magit-section-show (magit-current-section))
                         (recenter)
                         t)
                 do (condition-case nil
                        (magit-section-forward)
                      (error (cl-return (magit-status-goto-initial-section-1)))))))))

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

(defvar unpackaged/flex-fill-paragraph-column nil
  "Last fill column used in command `unpackaged/flex-fill-paragraph'.")

(defun unpackaged/flex-fill-paragraph (&optional fewer-lines unfill)
  "Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only
bound around calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the
default `fill-column'.

When called repeatedly, increase `fill-column' until filling
changes.

With one universal prefix, increase `fill-column' until the
number of lines is reduced.  With two, unfill completely."
  (interactive "P")
  (let* ((fewer-lines (or fewer-lines (equal current-prefix-arg '(4))))
         (unfill (or unfill (equal current-prefix-arg '(16))))
         (fill-column
          (cond (unfill (setf unpackaged/flex-fill-paragraph-column nil)
                        most-positive-fixnum)
                (t (setf unpackaged/flex-fill-paragraph-column
                         (if (equal last-command this-command)
                             (or (unpackaged/flex-fill-paragraph--next-fill-column fewer-lines)
                                 fill-column)
                           fill-column))))))
    (fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun unpackaged/flex-fill-paragraph--next-fill-column (&optional fewer-lines)
  "Return next `fill-column' value.
If FEWER-LINES is non-nil, reduce the number of lines in the
buffer, otherwise just change the current paragraph."
  ;; This works well, but because of all the temp buffers, sometimes when called
  ;; in rapid succession, it can cause GC, which can be noticeable.  It would be
  ;; nice to avoid that.  Note that this has primarily been tested on
  ;; `emacs-lisp-mode'; hopefully it works well in other modes.
  (let* ((point (point))
         (source-buffer (current-buffer))
         (mode major-mode)
         (fill-column (or unpackaged/flex-fill-paragraph-column fill-column))
         (old-fill-column fill-column)
         (hash (unless fewer-lines
                 (buffer-hash)))
         (original-num-lines (when fewer-lines
                               (line-number-at-pos (point-max)))))
    (with-temp-buffer
      (delay-mode-hooks
        (funcall mode))
      (setq-local fill-column old-fill-column)
      (insert-buffer-substring source-buffer)
      (goto-char point)
      (cl-loop while (fill-paragraph)
               ;; If filling doesn't change after 100 iterations, abort by returning nil.
               if (> (- fill-column old-fill-column) 100)
               return nil
               else do (cl-incf fill-column)
               while (if fewer-lines
                         (= original-num-lines (line-number-at-pos (point-max)))
                       (string= hash (buffer-hash)))
               finally return fill-column))))

;; smerge-mode instead of ediff
(defun my/save-and-bury ()
  (interactive)
  (save-buffer)
  (bury-buffer))
(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color orange :hint nil :post (smerge-auto-leave))
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
    ("ZZ" my/save-and-burry
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

(use-package anzu
  :diminish anzu-mode
  :ensure t
  :init
  (global-anzu-mode +1))

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

(defalias 'auto-tail-revert-mode 'tail-mode)

(use-package saveplace
  :ensure t
  :custom
  (select-enable-clipboard t)
  (select-enable-primary t)
  (save-interprogram-paste-before-kill t)
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (save-place-file (concat user-emacs-directory "places"))
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  :config
  (save-place-mode +1))

;; From http://steckerhalter.co.vu/steckemacs.html
;; Make isearch-forward put the cursor at the start of the search, not the end.
(defun my-isearch-goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-isearch-goto-match-beginning)

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
(add-hook 'c++-ts-mode
          (lambda ()
            (when (s-ends-with? "_test.cc" buffer-file-name)
              (setq-local
               imenu-generic-expression
               '(("Class" "^\\(template[        ]*<[^>]+>[      ]*\\)?\\(class\\|struct\\)[     ]+\\([[:alnum:]_]+\\(<[^>]+>\\)?\\)\\([         \n]\\|\\\\\n\\)*[:{]" 3)
                 ("Test" "^ *TEST\\(?:_F\\)?([^,]+,\n? *\\(.+\\)) {$" 1))))))

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
    (setq deft-default-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filter-string-for-filename t)
    (setq deft-use-filename-as-title t)))

;; eww browsing inside emacs. Much better than w3m!
(setq browse-url-browser-function 'eww-browse-url)
;; Consider adding in unpackaged/eww-imenu-index
;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#eww-imenu-support

(require 'subr-x)

(use-package which-key
  :diminish which-key-mode
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
  :bind (:map prog-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map c++-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map c-mode-map
              ("C-k" . gmbuell-smart-kill-line)
              :map go-mode-map
              ("C-k" . gmbuell-smart-kill-line))
  :diminish smartparens-mode
  :diminish smartparens-global-mode
  :custom
  (sp-override-key-bindings '(("C-<right>" . sp-slurp-hybrid-sexp)
                              ("C-<left>" . sp-dedent-adjust-sexp)))
  :init
  (smartparens-global-mode t)
  ;; smartparens-mode is incredibly difficult to diminish.
  (add-hook 'smartparens-mode-hook (lambda () (diminish 'smartparens-mode)))
  ;; Need to set up smartparens bindings in modes that combobulate doesn't
  ;; support.
  ;; (sp-use-smartparens-bindings)
  ;; Fix forward slurp spacing
  ;; https://github.com/Fuco1/smartparens/issues/297
  (sp-local-pair 'c-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*")
  (sp-local-pair 'c++-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*)")
  (sp-local-pair 'go-mode "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

;; For chromebook:
(global-set-key (kbd "<deletechar>") 'backward-kill-word)

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((xml . ("https://github.com/tree-sitter-grammars/tree-sitter-xml" "v0.7.0" "xml/src"))
							 (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0"))
							 (starlark . ("https://github.com/tree-sitter-grammars/tree-sitter-starlark" "v1.3.0"))
							 (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make" "v1.1.1"))
							 (thrift . ("https://github.com/tree-sitter-grammars/tree-sitter-thrift" "main"))
							 (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
							 (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.5"))
							 (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "master"))
							 (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
							 (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
               (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    ;; Make sure to git clone https://github.com/mickeynp/combobulate
    :load-path "lisp/combobulate"
		:demand t
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))))

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; (use-package solaire-mode
;;   :ensure t
;;   :init
;;   (solaire-global-mode +1))

(defadvice custom-theme-load-confirm (around no-query-safe-theme activate) t)

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
  (yas-global-mode 1))

(with-eval-after-load 'yasnippet
  (defun my/yas-docker-compose-bp ()
    (interactive)
    (yas-expand-snippet (yas-lookup-snippet "dockercomposebp" 'yaml-ts-mode))))

(use-package autoinsert
  :after (yasnippet)
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-alist nil) ;; remove this like to restore defaults
  (add-to-list 'auto-insert-alist  '("docker-compose.yaml$" . [my/yas-docker-compose-bp])))

(use-package yasnippet-snippets
  :ensure yasnippet-snippets
  :after (yasnippet))

;; persistent abbreviation file
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
;; Disable abbrev-mode
(abbrev-mode -1)
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
        org-src-tab-acts-natively t
				org-todo-keywords
				'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
	:bind (("C-c a" . org-agenda)
				 ("C-c c" . org-capture)
				 ("C-c b" . org-store-link))
  :config
  ;; (bind-key "C-c SPC" 'ace-jump-mode org-mode-map)
  ;; Add shortcut to recalculate table
  ;; (bind-key "M-r" '(lambda () (interactive)(org-table-recalculate t)) org-mode-map)
  (defun org-babel-execute:eshell (body _params)
    (with-temp-buffer
      (eshell-command body t)
      (buffer-string)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (comint . t)
     (sqlite . t)
     (emacs-lisp . t)
     (eshell . t)
     ))
  (require 'org-tempo))

(use-package ob-async
  :ensure t)

(use-package walkman
  :ensure t
  :config
  (setq walkman-keep-headers t)
  :bind (:map org-mode-map
              ("C-c w t" . walkman-transient)
              ("C-c w p" . walkman-at-point)
              ))

(use-package symbol-overlay
  :ensure t
  :init
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all))

(use-package symbol-overlay-mc
  :ensure t
  :after (symbol-overlay)
  :bind (("M-a" . symbol-overlay-mc-mark-all)))

(use-package casual
  :ensure t)

(use-package casual-symbol-overlay
  :ensure t
  :after (casual)
  :init
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
  :config
  (symbol-overlay-mc-insert-into-casual-tmenu))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package vertico
  :ensure t
	:demand t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed))))

(use-package vertico-directory
  :init
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package dirvish
  :demand t
  :ensure t
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-side)
   :map dirvish-mode-map          ; Dirvish inherits `dired-mode-map'
   ("o" . dired-up-directory)     ; So you can adjust dired bindings here
   ("?"   . dirvish-dispatch)     ; contains most of sub-menus in dirvish extensions
   ("." . dired-create-empty-file)
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-history-go-forward)
   ("b"   . dirvish-history-go-backward)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-setup-menu)   ; `st' toggles mtime, `ss' toggles file size, etc.
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("r"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-file-info-menu)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                     "Home")
     ("r" "~/repo/"                "repo")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state collapse file-size))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"))

;; Completion
;; headlong might be useful for bookmark jumping

;; Could also try out restricto as an alternative
;; https://github.com/oantolin/restricto
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(use-package prescient
  :ensure t
  :custom
  (completion-preview-sort-function #'prescient-completion-sort)
  (completion-styles '(prescient))
  :config
  (prescient-persist-mode 1))
(use-package vertico-prescient
  :ensure t
  :after (prescient vertico)
  :config
  (vertico-prescient-mode 1))
(use-package corfu-prescient
  :ensure t
  :after (prescient corfu)
  :config
  (corfu-prescient-mode 1))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(substring flex basic))
;;   (orderless-smart-case t))
;;   ;; :config
;;   ;; Allow space in minibuffer completions
;;   ;; (let ((map minibuffer-local-completion-map))
;;   ;;   (define-key map (kbd "SPC") nil)))

;; (defun flex-if-twiddle (pattern _index _total)
;;   (when (string-suffix-p "~" pattern)
;;     `(orderless-flex . ,(substring pattern 0 -1))))

;; (defun first-initialism (pattern index _total)
;;   (if (= index 0) 'orderless-initialism))

;; (defun without-if-bang (pattern _index _total)
;;   (cond
;;    ((equal "!" pattern)
;;     '(orderless-literal . ""))
;;    ((string-prefix-p "!" pattern)
;;     `(orderless-without-literal . ,(substring pattern 1)))))

;; (setq orderless-matching-styles '(orderless-regexp)
;;       orderless-style-dispatchers '(first-initialism
;;                                     flex-if-twiddle
;;                                     without-if-bang))

;; Try savehist for minibuffer history
(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

(defun with-minibuffer-keymap (keymap)
  (lambda (fn &rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap keymap (current-local-map))))
      (apply fn args))))

(use-package embark
  :ensure t
	:demand t
  :after (vertico)
  :bind
  (("M-." . embark-act)          ;; Could also be embark-dwim for more of a
   ;; direct xref substitute
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Make embark behave like helm
  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") 'abort-recursive-edit)
      map))

  (advice-add 'embark-completing-read-prompter :around
              (with-minibuffer-keymap embark-completing-read-prompter-map))
  (define-key vertico-map (kbd "TAB") 'embark-act-with-completing-read)

  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
	(defun embark-kmacro-target ()
		"Target a textual kmacro in braces."
		(save-excursion
			(let ((beg (progn (skip-chars-backward "^{}\n") (point)))
						(end (progn (skip-chars-forward "^{}\n") (point))))
				(when (and (eq (char-before beg) ?{) (eq (char-after end) ?}))
					`(kmacro ,(buffer-substring-no-properties beg end)
									 . (,(1- beg) . ,(1+ end)))))))
	(add-to-list 'embark-target-finders 'embark-kmacro-target)

	(defun embark-kmacro-run (arg kmacro)
		(interactive "p\nsKmacro: ")
		(kmacro-call-macro arg t nil (kbd kmacro)))

	(defun embark-kmacro-save (kmacro)
		(interactive "sKmacro: ")
		(kmacro-push-ring)
		(setq last-kbd-macro (kbd kmacro)))

	(defun embark-kmacro-name (kmacro name)
		(interactive "sKmacro: \nSName: ")
		(let ((last-kbd-macro (kbd kmacro)))
			(kmacro-name-last-macro name)))

	(defun embark-kmacro-bind (kmacro)
		(interactive "sKmacro: \n")
		(let ((last-kbd-macro (kbd kmacro)))
			(kmacro-bind-to-key nil)))

	(defvar-keymap embark-kmacro-map
		:doc "Actions on kmacros."
		:parent embark-general-map
		"RET" #'embark-kmacro-run
		"s" #'embark-kmacro-save
		"n" #'embark-kmacro-name
		"b" #'embark-kmacro-bind)

	(add-to-list 'embark-keymap-alist '(kmacro . embark-kmacro-map))
	)

(defun gmbuell/consult-ripgrep (&optional dir initial)
  "Start consult-ripgrep search in the current directory"
  (interactive "P")
  (consult-ripgrep (or dir default-directory) initial)
  )

(defun wrapper/consult-ripgrep (&optional dir given-initial)
	"Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
	(interactive "P")
	(let ((initial
				 (or given-initial
						 (when (use-region-p)
							 (buffer-substring-no-properties (region-beginning) (region-end))))))
		(gmbuell/consult-ripgrep dir initial)))

;; ripgrep as grep
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

(use-package consult
  :ensure t
	:demand t
  :bind (;; ("M-i" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . gmbuell/consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
				 :map region-bindings-mode-map
         ("r" . wrapper/consult-ripgrep)
         )
  :init
  (require 'esh-mode)
  (bind-key* "C-c C-l" 'consult-history eshell-mode-map)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")
  (defvar consult--source-recent-file-hidden
    `(:hidden t :narrow (?a . "File") :enabled ,(lambda () (consult--project-root)) ,@consult--source-recent-file)
    "Like `consult--source-recent-file' but hidden by default.")
  (defvar consult--source-buffer-hidden
    `(:hidden t :narrow (?a . "Buffer") :enabled ,(lambda () (consult--project-root)) ,@consult--source-buffer)
    "Like `consult--source-buffer' but hidden by default.")
  ;; Define non-hidden buffer source for non-project contexts
  (defvar consult--source-non-project-buffer
    `(:enabled ,(lambda () (not (consult--project-root)))
               :narrow (?b . "Buffer")
               ,@consult--source-buffer)
    "Buffer candidate source for when not in a project.")

  ;; Define non-hidden recent file source for non-project contexts
  (defvar consult--source-non-project-recent-file
    `(:enabled ,(lambda () (not (consult--project-root)))
               :narrow (?f . "File")
               ,@consult--source-recent-file)
    "Recent file candidate source for when not in a project.")
  ;; Excludes star buffers
  (defvar consult--source-project-useful-buffer
    `( :name     "Project Buffer"
       :narrow   ?b
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :enabled  ,(lambda () (consult--project-root))
       :items
       ,(lambda ()
          (when-let (root (consult--project-root))
            (consult--buffer-query :sort 'visibility
                                   :directory root
                                   :as #'consult--buffer-pair
                                   :exclude spacemacs-useless-buffers-regexp))))
    "Project buffer candidate source for `consult-buffer'.")
  ;; Define a new source for useful buffers
  (defvar consult--source-useful-buffer
    `(:name     "Useful Buffer"
                :narrow   ?u
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :state    ,#'consult--buffer-state
                :items
                ,(lambda ()
                   (consult--buffer-query
                    :sort 'visibility
                    :as #'consult--buffer-pair
                    :predicate
                    (lambda (buf)
                      (let ((buf-name (buffer-name buf)))
                        (or (string-match-p "\\*eat\\*" buf-name)
                            ;; Add more useful buffer patterns here as needed
                            ;; Example: (string-match-p "\\*shell\\*" buf-name)
                            ;; Example: (string-match-p "\\*eshell\\*" buf-name)
                            ))))))
    "Useful buffer candidate source for `consult-buffer'.")
  (setq consult-buffer-sources '(consult--source-buffer-hidden
                                 consult--source-non-project-buffer
                                 consult--source-project-useful-buffer
                                 consult--source-project-recent-file
                                 consult--source-useful-buffer
                                 consult--source-recent-file-hidden
                                 consult--source-non-project-recent-file))
  )

(defun gmbuell/consult-ripgrep-up-directory ()
	(interactive)
	(let ((parent-dir (file-name-directory (directory-file-name default-directory))))
		(when parent-dir
      (run-at-time 0 nil
                   #'consult-ripgrep
                   parent-dir
                   (ignore-errors
                     (buffer-substring-no-properties
                      (1+ (minibuffer-prompt-end)) (point-max))))))
	(minibuffer-quit-recursive-edit))

(defvar gmbuell/consult-ripgrep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-o") #'gmbuell/consult-ripgrep-up-directory)
    map))
(advice-add 'consult-ripgrep :around
            (with-minibuffer-keymap gmbuell/consult-ripgrep-map))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defvar consult--previous-point nil
  "Location of point before entering minibuffer.
Used to preselect nearest headings and imenu items.")

(defun consult--set-previous-point (&optional arg1 arg2)
  "Save location of point. Used before entering the minibuffer."
  (setq consult--previous-point (point)))

(advice-add #'consult-org-heading :before #'consult--set-previous-point)
(advice-add #'consult-outline :before #'consult--set-previous-point)

(advice-add #'vertico--update :after #'consult-vertico--update-choose)

(defun consult-vertico--update-choose (&rest _)
  "Pick the nearest candidate rather than the first after updating candidates."
  (when (and consult--previous-point
             (memq current-minibuffer-command
                   '(consult-org-heading consult-outline)))
    (setq vertico--index
          (max 0 ; if none above, choose the first below
               (1- (or (seq-position
                        vertico--candidates
                        consult--previous-point
                        (lambda (cand point-pos) ; counts on candidate list being sorted
                          (> (cl-case current-minibuffer-command
                               (consult-outline
                                (car (consult--get-location cand)))
                               (consult-org-heading
                                (get-text-property 0 'consult--candidate cand)))
                             point-pos)))
                       (length vertico--candidates))))))
  (setq consult--previous-point nil))

(require 'keymap) ;; keymap-substitute requires emacs version 29.1?
(require 'cl-seq)

(keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
(cl-nsubstitute-if
 '(consult-ripgrep "Find regexp")
 (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
 project-switch-commands)

(add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))

(use-package hrm
	:bind (("C-c C-h" . hrm-notes)))

(use-package consult-dir
  :ensure t
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :init
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))

(use-package consult-eglot
  :ensure t
  :after (consult))
(use-package consult-eglot-embark
  :ensure t
  :after (consult-eglot)
  :init
  (consult-eglot-embark-mode))

(use-package disproject
  :ensure t
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; wgrep mode to edit grep buffers (produced by embark-export)
(use-package wgrep
  :ensure t)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1)
         ;;("C-c C-j" . avy-resume)
         ("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch)
         )
  :init
  (avy-setup-default)
  (setq avy-style 'at)
  (setq avy-background t)
  (setq avy-keys (number-sequence ?a ?z)))

(use-package ace-window
  :after (embark)
  :ensure t
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (defvar-keymap my/window-prefix-map
    :doc "Keymap for various window-prefix maps"
    :suppress 'nodigits
    "o" #'ace-window-prefix
    "0" #'ace-window-prefix
    "1" #'same-window-prefix
    "2" #'split-window-vertically
    "3" #'split-window-horizontally
    "4" #'other-window-prefix
    "5" #'other-frame-prefix
    "6" #'other-tab-prefix
    "t" #'other-tab-prefix)
  ;; Look up the key in `my/window-prefix-map' and call that function first.
  ;; Then run the default embark action.
  (cl-defun my/embark--call-prefix-action (&rest rest &key run type &allow-other-keys)
    (when-let ((cmd (keymap-lookup
                     my/window-prefix-map
                     (key-description (this-command-keys-vector)))))
      (funcall cmd))
    (funcall run :action (embark--default-action type) :type type rest))
  ;; Dummy function, will be overridden by running `embark-around-action-hooks'
  (defun my/embark-set-window () (interactive))

  ;; When running the dummy function, call the prefix action from above
  (setf (alist-get 'my/embark-set-window embark-around-action-hooks)
        '(my/embark--call-prefix-action))
  (setf (alist-get 'buffer embark-default-action-overrides) #'pop-to-buffer-same-window
        (alist-get 'file embark-default-action-overrides) #'find-file
        (alist-get 'bookmark embark-default-action-overrides) #'bookmark-jump
        (alist-get 'library embark-default-action-overrides) #'find-library)
  (map-keymap (lambda (key cmd)
                (keymap-set embark-general-map (key-description (make-vector 1 key))
                            #'my/embark-set-window))
              my/window-prefix-map))

(use-package f
  :ensure t)

;; Version of ora-company-number for corfu
(defun ora-corfu-number ()
  "Forward to `corfu-insert'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" (car corfu--input) k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    corfu--candidates)
        (self-insert-command 1)
      (corfu--goto (string-to-number k))
      (corfu-insert))))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; Remember to disable corfu-auto if I'm using copilot
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  (setq tab-always-indent 'complete)
  (let ((map corfu-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-corfu-number))
     (number-sequence 0 9))))

(use-package corfu-indexed
  :after (corfu)
  :init
  (corfu-indexed-mode 1))

(use-package corfu-history
  :after (corfu)
  :init
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package popon
	:load-path "lisp/emacs-popon"
	:demand t)
(use-package corfu-terminal
	:load-path "lisp/emacs-corfu-terminal"
	:if (not (display-graphic-p))
	:demand t
	:after (corfu popon)
  :config
	(corfu-terminal-mode +1))

(use-package flymake-popon
	:load-path "lisp/emacs-flymake-popon"
	:after (popon posframe)
	:demand t
  :diminish flymake-popon-mode
  :config
  (global-flymake-popon-mode))


;; Temporarily disable copilot mode
;; (when (not (file-exists-p "/google/bin/releases/"))
;;   (use-package copilot
;;     :quelpa (copilot :fetcher github
;;                      :repo "zerolfx/copilot.el"
;;                      :branch "main"
;;                      :files ("dist" "*.el"))
;;     :init
;;     (add-hook 'prog-mode-hook 'copilot-mode)
;;     :config
;;     (defun rk/no-copilot-mode ()
;;       "Helper for `rk/no-copilot-modes'."
;;       (copilot-mode -1))

;;     (defvar rk/no-copilot-modes '(shell-mode
;;                                   inferior-python-mode
;;                                   eshell-mode
;;                                   term-mode
;;                                   vterm-mode
;;                                   comint-mode
;;                                   compilation-mode
;;                                   debugger-mode
;;                                   dired-mode-hook
;;                                   compilation-mode-hook
;;                                   flutter-mode-hook
;;                                   minibuffer-mode-hook
;;                                   shelldon-mode-hook)
;;       "Modes in which copilot is inconvenient.")

;;     (defun rk/copilot-disable-predicate ()
;;       "When copilot should not automatically show completions."
;;       (or rk/copilot-manual-mode
;;           (member major-mode rk/no-copilot-modes)
;;           ;; Need to figure out what the equivalent for corfu is if I want this behavior
;;           ;;(company--active-p)
;;           ))

;;     (defvar rk/copilot-manual-mode nil
;;       "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

;;     (defun rk/copilot-change-activation ()
;;       "Switch between three activation modes:
;; - automatic: copilot will automatically overlay completions
;; - manual: you need to press a key (M-C-<return>) to trigger completions
;; - off: copilot is completely disabled."
;;       (interactive)
;;       (if (and copilot-mode rk/copilot-manual-mode)
;;           (progn
;;             (message "deactivating copilot")
;;             (global-copilot-mode -1)
;;             (setq rk/copilot-manual-mode nil))
;;         (if copilot-mode
;;             (progn
;;               (message "activating copilot manual mode")
;;               (setq rk/copilot-manual-mode t))
;;           (message "activating copilot mode")
;;           (global-copilot-mode))))

;;     (defun rk/copilot-complete-or-accept ()
;;       "Command that either triggers a completion or accepts one if one
;; is available. Useful if you tend to hammer your keys like I do."
;;       (interactive)
;;       (if (copilot--overlay-visible)
;;           (progn
;;             (copilot-accept-completion)
;;             (open-line 1)
;;             (next-line))
;;         (copilot-complete)))

;;     (defun rk/copilot-quit ()
;;       "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
;; cleared, make sure the overlay doesn't come back too soon."
;;       (interactive)
;;       (when copilot--overlay
;;         (setq pre-copilot-disable-predicates copilot-disable-predicates)
;;         (setq copilot-disable-predicates (list (lambda () t)))
;;         (copilot-clear-overlay)
;;         (run-with-idle-timer
;;          1.0
;;          nil
;;          (lambda ()
;;            (setq copilot-disable-predicates pre-copilot-disable-predicates)))
;;         ))

;;     (defun rk/copilot-complete-if-active (next-func n)
;;       (let ((completed (when copilot-mode (copilot-accept-completion))))
;;         (unless completed (funcall next-func n))))
;;     (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)
;;     ;; global keybindings
;;     ;; (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)
;;     ;; dabbrev-expand isn't that good.
;;     (global-set-key [remap dabbrev-expand] #'rk/copilot-complete-or-accept)
;;     ;; Do copilot-quit when pressing C-g
;;     ;; (advice-add 'keyboard-quit :before #'rk/copilot-quit)
;;     ;; complete by pressing right or tab but only when copilot completions are
;;     ;; shown. This means we leave the normal functionality intact.
;;     ;; (advice-add 'right-char :around #'rk/copilot-complete-if-active)
;;     ;; (advice-add 'indent-for-tab-command :around #'rk/copilot-complete-if-active)
;;     :bind (:map copilot-completion-map
;;                 ("C-g"  . rk/copilot-quit)
;;                 ("C-f" . copilot-accept-completion)
;;                 ("<right>" . copilot-accept-completion)
;;                 ("M-<right>" . copilot-accept-completion-by-word)
;;                 ("M-f" . copilot-accept-completion-by-word)
;;                 ("C-e" . copilot-accept-completion-by-line)
;;                 ("<end>" . copilot-accept-completion-by-line)
;;                 ("M-n" . copilot-next-completion)
;;                 ("M-p" . copilot-previous-completion))))

;; Consider disabling complete if using copilot
(setq tab-always-indent 'complete)
(defun gmbuell-indent-or-complete-common (arg)
  "Indent the current line or region, or complete the common part."
  (interactive "P")
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((memq indent-line-function
          '(indent-relative indent-relative-maybe))
    (completion-at-point))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (indent-for-tab-command arg)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (completion-at-point))))))

(bind-key "TAB" 'gmbuell-indent-or-complete-common prog-mode-map)

(use-package flymake
  :ensure t
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  :hook (find-file . flymake-find-file-hook)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(use-package eglot
  :after (yasnippet)
  :ensure t
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode go-mode go-ts-mode python-mode python-ts-mode protobuf-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake))
  :config
	(add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode c-ts-mode) . my-clangd-command))
  (when (file-exists-p "/google/bin/releases/")
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
       . ("/google/bin/releases/grok/tools/kythe_languageserver" "--google3"))))
  ;; "-index-file=/usr/local/google/home/gmbuell/index.idx")
  ;;(add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("/google/bin/releases/grok/tools/kythe_languageserver" "--google3")))
  )

(eval-when-compile
  (require 'compile))

(use-package cc-mode)

(use-package compile-multi
  :ensure t
  :demand t
  :bind (:map prog-mode-map
              ("C-c C-c" . compile-multi)
              :map c++-mode-map
              ("C-c C-c" . compile-multi))
  :init
  (add-hook 'c++-ts-mode (progn (require 'c-ts-mode) (bind-key "C-c C-c" 'compile-multi c++-ts-mode-map)))
  :config
  (defun my/compile-multi-bazel-available-targets ()
    "Generate Bazel targets for compile-multi based on available targets and rules."
    (when-let* ((file-name (or buffer-file-name default-directory))
                (workspace-root (bazel--workspace-root file-name))
                (package-dir (bazel--package-directory file-name workspace-root))
                (package-name (bazel--package-name package-dir workspace-root))
                (build-file (bazel--locate-build-file package-dir)))
      (with-temp-buffer
        (insert-file-contents build-file)
        (let ((targets nil)
              (rule-types (make-hash-table :test 'equal)))

          ;; First pass: collect rule types
          (goto-char (point-min))
          (while (re-search-forward "\\([a-zA-Z0-9_]+\\)(\\s-*\n?\\s-*name\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
            (let ((rule-type (match-string 1))
                  (target-name (match-string 2)))
              (puthash target-name rule-type rule-types)))

          ;; Second pass: create appropriate actions based on target name and rule type
          (maphash
           (lambda (target-name rule-type)
             (cond
              ;; Test targets (_test suffix)
              ((string-match-p "_test$" target-name)
               (push (cons (format "bazel:test:%s" target-name)
                           `(:command ,(format "bazel test %s:%s" package-name target-name)
                                      :annotation ,(format "Test %s (%s)" target-name rule-type)))
                     targets))

              ;; pkg_tar targets - no actions
              ((string= rule-type "pkg_tar")
               nil)

              ;; oci_image targets - no actions
              ((string= rule-type "oci_image")
               nil)

              ;; oci_push targets - only run action
              ((string= rule-type "oci_push")
               (push (cons (format "bazel:run:%s" target-name)
                           `(:command ,(format "bazel run %s:%s" package-name target-name)
                                      :annotation ,(format "Run %s (%s)" target-name rule-type)))
                     targets))

              ;; _lib targets - only build action
              ((string-match-p "_lib$" target-name)
               (push (cons (format "bazel:build:%s" target-name)
                           `(:command ,(format "bazel build %s:%s" package-name target-name)
                                      :annotation ,(format "Build %s (%s)" target-name rule-type)))
                     targets))

              ;; All other targets - build and run actions
              (t
               (push (cons (format "bazel:build:%s" target-name)
                           `(:command ,(format "bazel build %s:%s" package-name target-name)
                                      :annotation ,(format "Build %s (%s)" target-name rule-type)))
                     targets)
               (push (cons (format "bazel:run:%s" target-name)
                           `(:command ,(format "bazel run %s:%s" package-name target-name)
                                      :annotation ,(format "Run %s (%s)" target-name rule-type)))
                     targets))))
           rule-types)

          ;; Add general package commands
          (append targets
                  (list
                   (cons "bazel:build-all"
                         `(:command ,(format "bazel build //%s/..." package-name)
                                    :annotation ,(format "Build all targets in //%s" package-name)))
                   (cons "bazel:test-all"
                         `(:command ,(format "bazel test //%s/..." package-name)
                                    :annotation ,(format "Test all targets in //%s" package-name)))))))))

  ;; Use project.el to find the project root for compile-multi
  (defun my/project-root-or-default ()
    "Return the project root or default directory."
    (if-let ((project (project-current)))
        (project-root project)
      default-directory))

  ;; Set compile-multi default directory to use project.el
  (setq compile-multi-default-directory #'my/project-root-or-default)

  ;; Setup compile-multi configuration for Bazel projects
  (push '((lambda ()
            (when buffer-file-name
              (bazel--workspace-root buffer-file-name)))
          ;; This function will generate targets when in a Bazel workspace
          my/compile-multi-bazel-available-targets)
        compile-multi-config))

(use-package consult-compile-multi
  :ensure t
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-embark
  :ensure t
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

(use-package projection
  :ensure t
  ;; Enable the `projection-hook' feature.
  :hook (after-init . global-projection-hook-mode)

  ;; Require projections immediately after project.el.
  :config
  (with-eval-after-load 'project
    (require 'projection))

  :config
  ;; Uncomment if you want to disable prompts for compile commands customized in .dir-locals.el
  ;; (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-install-project 'safe-local-variable #'stringp)

  ;; Access pre-configured projection commands from a keybinding of your choice.
  ;; Run `M-x describe-keymap projection-map` for a list of available commands.
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :ensure t
  ;; Allow interactively selecting available compilation targets from the current
  ;; project type.
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :ensure t
  :after embark
  :after projection-multi
  :demand t
  ;; Add the projection set-command bindings to `compile-multi-embark-command-map'.
  :config (projection-multi-embark-setup-command-map))

;; Tell eglot about go modules
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)
(use-package eldoc
  :diminish eldoc-mode)

(use-package bazel
  :ensure t
  :custom
  (bazel-command '("bazel"))
  ;; :bind (("C-c C-c" . compile))
  )

(use-package eshell
  :hook ((eshell-mode . (lambda () (setq-local corfu-auto nil
                                               corfu-quit-at-boundary t
                                               corfu-quit-no-match t))))
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  ;; (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  (eshell-term-name "xterm-256color"))

(use-package pcmpl-args
  :after pcomplete)

(use-package cape
  :ensure t
  :init
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'completion-at-point-functions #'cape-history)))
  )

(use-package pcmpl-args
  :ensure t)

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(require 'nadvice)
(defun do-nothing (orig-fun &rest args) t)

;; It may be possible to use avy to set marks for multiple cursors.
;; I.e. avy-push-mark
(use-package multiple-cursors
  :ensure t
  :demand t
  ;; Alternative bindings because hterm sucks at passing through keys
  :bind* (("M-m e" . mc/edit-lines)
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

(use-package treesit-fold
  :load-path "lisp/treesit-fold"
	:bind (("C-<return>" . treesit-fold-toggle))
	:config
	(setq treesit-fold-line-count-show t))

(use-package fold-this
	:ensure t
	:demand t
	:bind (:map region-bindings-mode-map
							("F" . fold-active-region-all)))

(use-package expand-region
	:ensure t
	:bind (("C-c e" . er/expand-region)
				 :map region-bindings-mode-map
				 ("u" . er/contract-region)
				 ("e" . er/expand-region)))

;; phi search works with multiple cursors But has weird behavior around trying
;; to move while in a search. Fortunately, multiple cursors mode seems to use it
;; automatically even if not set as default.
(use-package phi-search
	:ensure t
	:demand t)
(use-package phi-replace
	;; Doesn't have its own package.
	:requires (phi-search)
	:demand t)

;; Consider also placeholder
;; https://github.com/oantolin/placeholder
(use-package auto-yasnippet
	:after (yasnippet)
	:ensure t
	:bind (("M-w" . aya-create)
				 ("M-W" . aya-expand))
	;; Doesn't currently do anything because I've remapped M-w which is usually
	;; kill-ring-save.
	;; (use-package easy-kill :ensure t :config (global-set-key
	;; [remap kill-ring-save] 'easy-kill))
	:custom
	(aya-case-fold t))

(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode))

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
		pkg_tar oci_image oci_push
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
		tars repository remote_tags image
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
(defun eglot-format-buffer-before-save ()
	(add-hook 'before-save-hook #'eglot-format-buffer -10 t))
;; (defun eglot-organize-imports-before-save ()
;;   (add-hook 'before-save-hook
;;             (lambda ()
;;               (call-interactively 'eglot-code-action-organize-imports))
;;             nil t))

(use-package go-mode
	:ensure t
	:init
	(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
	(add-to-list 'auto-mode-alist '("\\.rl\\'" . go-mode))
	:hook ((go-mode . yas-minor-mode)
				 (go-mode . eglot-ensure)
				 ;; Switch to apheleia
				 ;; (go-mode . eglot-format-buffer-before-save)
				 (go-ts-mode . yas-minor-mode)
				 (go-ts-mode . eglot-ensure)
				 (go-mode . (lambda () (setq indent-tabs-mode t)))
				 (go-ts-mode . (lambda () (setq indent-tabs-mode t))))
	;; :config
	;; (defun my-custom-compile ()
	;;   "Compile using custom compile command."
	;;   (interactive)
	;;   (compile (cond ((string-match-p "\\machine.rl\\'" buffer-file-name)
	;;                   "ragel -Z -G2 machine.rl -o machine.go")
	;;                  ((string-match-p "_test\\.go\\'" buffer-file-name)
	;;                   "bazel test -- \\:all")
	;;                  (t
	;;                   "bazel build :all"))))
	;; :bind (("C-c C-c" . my-custom-compile))
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

(setq enable-recursive-minibuffers t)

(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)

(use-package link-hint
	:ensure t
	:bind (:map eshell-mode-map
							("C-c w" . link-hint-copy-link)))

;; Show hydras overlayed in the middle of the frame
(use-package hydra-posframe
	;; Only enable if we are running graphical mode.
	:if window-system
	:load-path "lisp/hydra-posframe"
	:hook (after-init . hydra-posframe-mode)
	:custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra
	:after hydra
	:ensure t)

;; A replacement for which-func-mode.
(use-package breadcrumb
	:load-path "lisp/breadcrumb"
	:demand t
	:config
	(breadcrumb-mode))

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

;; (use-package popper
;;   :ensure t
;;   :bind (("C-t"   . my/popper-toggle-or-eshell)
;;          :map help-mode-map
;;          ("q" . my/popper-quit)
;;          :map special-mode-map
;;          ("q" . my/popper-quit)
;;          )
;;   :init
;;   (setq popper-reference-buffers
;;         '(;;"\\*Messages\\*"
;;           ;; "Output\\*$"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           compilation-mode
;;           flymake-diagnostics-buffer-mode
;;           "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
;;           "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
;;           "^\\*term.*\\*$"   term-mode   ;term as a popup
;;           "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
;;           ;; "^\\*eldoc.*\\*$"  eldoc-mode  ; eldoc as a popup
;;           ))
;;   ;;(setq popper-group-function #'popper-group-by-project)
;;   (setq popper-group-function #'popper-group-by-directory)
;;   (popper-mode +1)
;;   (popper-echo-mode +1)
;;   (defun my/popper-quit ()
;;     "Kill popper popup if one is active, otherwise quit window"
;;     (interactive)
;;     (if popper-open-popup-alist
;;         (popper-kill-latest-popup)
;;       (quit-window)))
;;   (defun my/popper-toggle-or-eshell ()
;;     "Toggle popup or create new eshell if no buried popups exist."
;;     (interactive)
;;     (if popper-open-popup-alist
;;         (popper-toggle)
;;       (let ((group (when popper-group-function (funcall popper-group-function))))
;;         (if (null (alist-get group popper-buried-popup-alist nil nil 'equal))
;;             (eshell t)
;;           (popper-toggle))))))

;; From https://stackoverflow.com/questions/13009908/eshell-search-history
(defun my-eshell-previous-matching-input-from-input (arg)
	"Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
	(interactive "p")
	(if (not (memq last-command '(eshell-previous-matching-input-from-input
																eshell-next-matching-input-from-input)))
			;; Starting a new search
			(setq eshell-matching-input-from-input-string
						(buffer-substring (save-excursion (eshell-bol) (point))
															(point))
						eshell-history-index nil))
	(eshell-previous-matching-input
	 (regexp-quote eshell-matching-input-from-input-string)
	 arg))

;; override eshell-previous-matching-input-from-input, because it limits the search is from the beginning.
(advice-add 'eshell-previous-matching-input-from-input :override #'my-eshell-previous-matching-input-from-input)

(use-package eat
	:ensure t
	:hook (eshell-load . eat-eshell-visual-command-mode))

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
	:config
	(dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
		(diminish mode))
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
	:mode ("\\.epub\\'" . nov-mode))

;; Calendars should start with Monday as the first day of the week.
(setq calendar-week-start-day 1)

(defun date ()
	"Insert today's date in MM-DD-YY format."
	(interactive)
	(insert (format-time-string "%-m-%-d-%y")))

(use-package palaver
	:demand t
	:custom
	(palaver-main-window-min-width 80)
	:config
	(palaver-mode 1)
	:bind
	(("C-c k" . palaver-toggle-bottom-drawer)
	 ("C-c l" . palaver-toggle-right-drawer)
	 ("C-c m" . palaver-toggle-drawer-location)
	 ("C-x o" . palaver-other-window)))

;; prism for highlighting modes without good syntax hilighting
(use-package prism
	:load-path "lisp/prism.el")

(use-package vundo
	:ensure t)

(use-package dogears
	:load-path "lisp/dogears.el"
	:demand t
	:after (consult)
	:bind (:map global-map
							("M-g d" . dogears-go)
							("M-g M-b" . dogears-back)
							("M-g M-f" . dogears-forward)
							("M-g M-d" . dogears-list))
	:init
	(add-hook 'prog-mode-hook #'dogears-mode)
	:config
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
		(consult--multi '(consult--source-dogears))))

(use-package mini-echo
	:ensure t
	:demand t
	:custom
	(mini-echo-right-padding 1)
	;; Other rules: project
	(mini-echo-persistent-rule '(:long
															 ("major-mode" "buffer-name" "vcs" "flymake")
															 :short ("buffer-name" "flymake")))
	:init
	(mini-echo-mode)
	(set-face-attribute 'mini-echo-minibuffer-window nil
											:background "#222323"))

(use-package apheleia
	:ensure t
	:demand t
	:init
	(apheleia-global-mode +1))

(defun my-minuet-endpoint-reachable-p ()
  "Check if minuet endpoint is reachable and my-minuet-enabled is true."
  (when my-minuet-enabled
    (condition-case nil
        (let* ((url-request-method "HEAD")
               (url-request-timeout 2)
               (status (url-http-symbol-value-in-buffer
                        'url-http-response-status
                        (url-retrieve-synchronously my-minuet-endpoint nil nil 2))))
          (and status (>= status 200) (< status 400)))
      (error nil))))

(use-package minuet
  :ensure t
	:if (and my-minuet-enabled (my-minuet-endpoint-reachable-p))
  :bind
  (("M-/" . #'minuet-show-suggestion)
   :map minuet-active-mode-map
	 ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("TAB" . #'minuet-accept-suggestion)
   ("C-e" . #'minuet-accept-suggestion-line)
   ("C-g" . #'minuet-dismiss-suggestion))

  :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-n-completions 1)
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-context-window 4096)

  ;; Apply the custom variables
  (plist-put minuet-openai-fim-compatible-options :end-point my-minuet-endpoint)
  (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
  (plist-put minuet-openai-fim-compatible-options :api-key my-minuet-api-key)
  (plist-put minuet-openai-fim-compatible-options :model my-minuet-model)

  ;; Disable suffix
  (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)

  ;; Set model-specific prompt function
  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :prompt
   (cond
    ((string= my-minuet-model "codegemma")
     (lambda (ctx)
       (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
               (plist-get ctx :language-and-tab)
               (plist-get ctx :before-cursor)
               (plist-get ctx :after-cursor))))

    ((string= my-minuet-model "deepseekcoder-v2")
     (lambda (ctx)
       (format "<｜fim▁begin｜>%s\n%s<｜fim▁hole｜>%s<｜fim▁end｜>"
               (plist-get ctx :language-and-tab)
               (plist-get ctx :before-cursor)
               (plist-get ctx :after-cursor))))

    (t  ;; Default to starcoder2 format
     (lambda (ctx)
       (format "<fim_prefix>%s\n%s<fim_suffix>%s<fim_middle>"
               (plist-get ctx :language-and-tab)
               (plist-get ctx :before-cursor)
               (plist-get ctx :after-cursor)))))
   :template)

  ;; Set common options
  (minuet-set-optional-options minuet-openai-fim-compatible-options :stop ["\n\n" "}" "<|endoftext|>"])
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :temperature 0))

(require 'server)
(unless (server-running-p) (server-start))
(setenv "EDITOR" "emacsclient")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
