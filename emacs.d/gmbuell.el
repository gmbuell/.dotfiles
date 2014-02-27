(setq gc-cons-threshold 20000000)

(load-theme 'solarized-dark t)
(set-default-font "DejaVu Sans Mono-11")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)

(add-to-list 'load-path "~/.emacs.d/gmbuell")
(require 'google)

(require 'auto-complete-autoloads)
(require 'auto-complete-config)
(ac-config-default)

(require 'ess-site)

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(require 'mustache-mode)
(require 'markdown-mode)
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

(defun flatten-assoc-tree (tree pred)
  "Returns an alist of only (key . leaf) pairs in TREE. PRED
 determines whether a value is a sub-alist or a leaf."
  (flet ((inner (lst)
                (mapcan (lambda (elt)
                          (cond ((atom elt) nil)
                                ((funcall pred elt) (inner elt))
                                (t (list elt))))
                        lst)))
    (inner tree)))

(defun ido-imenu ()
  "Queries with `ido-completing-read' a symbol in the buffer's
 imenu index, then jumps to that symbol's location."
  (interactive)
  (goto-char
   (let ((lst (nreverse (flatten-assoc-tree (imenu--make-index-alist) 'imenu--subalist-p))))
     (cdr (assoc (ido-completing-read "Symbol: " (mapcar 'car lst)) lst)))))
(global-set-key (kbd "C-x C-i") 'ido-imenu)

(require 'git-gutter)
(global-git-gutter-mode t)

;; w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t)

(require 'magit)

;; From http://www.emacswiki.org/emacs/OccurMode
(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
        (set-buffer (get-buffer "*Occur*"))
        (goto-char (point-min))
        (toggle-read-only 0)
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

;;(require 'sr-speedbar)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; For dired-jump
(require 'dired-x)

(require 'jabber-autoloads)
(require 'jabber)
(setq jabber-account-list
      '(("gmbuell@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(global-set-key (kbd "C-c t") 'ido-goto-symbol)

(require 'coffee-mode)
(setq coffee-tab-width 2)
(define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-file)

;;(require 'smartparens)
;;(require 'smartparens-config)
;;(smartparens-global-mode t)
;;(show-smartparens-global-mode +1)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(global-unset-key (kbd "<f1>"))
(define-key global-map (kbd "<f1>") 'multi-term-dedicated-toggle)

(require 'helm-config)
(require 'helm-ls-git)
(global-set-key (kbd "C-c h") 'helm-ls-git-ls)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-p") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c C-n") 'ace-jump-line-mode)

(require 'rainbow-delimiters)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)
(add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)

(server-start)
