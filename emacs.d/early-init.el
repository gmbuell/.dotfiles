(setq package-enable-at-startup nil)

;; Maximize GC threshold during init, restore after startup
(setq gc-cons-threshold most-positive-fixnum)

;; Skip regex matching on file loads during init
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 100000000)
    (setq file-name-handler-alist default-file-name-handler-alist)))

;; Disable UI chrome before frames are drawn
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) initial-frame-alist)
(push '(tool-bar-lines . 0) initial-frame-alist)
(push '(vertical-scroll-bars) initial-frame-alist)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
