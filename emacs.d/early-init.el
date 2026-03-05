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
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq scroll-bar-mode nil)
(setq tooltip-mode nil)
