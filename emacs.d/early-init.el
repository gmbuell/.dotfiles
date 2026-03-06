(setq package-quickstart t)

;; In batch mode, Emacs skips the normal startup sequence that calls
;; package-activate-all between early-init and init. Do it ourselves.
(when noninteractive
  (require 'package)
  (package-activate-all))

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
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
