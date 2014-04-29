;;; gmbuell-ruby --- Ruby specific package setup and customization.
;;; Commentary:
;; Sets up Emacs for Ruby. You should familiarize yourself with the
;; following packages (in order of importance):
;;
;; Robe (Code navigation, documentation lookup and completion):
;; https://github.com/dgutov/robe
;;
;; Use "C-h R" to do ri documentation lookup with helm in a ruby file.
;;
;; Install rubocop (gem install rubycop) and you will automatically
;; get ruby code linting via flycheck:
;; https://github.com/bbatsov/rubocop
;;
;; Start an interactive ruby console (like irb) with:
;; M-x inf-ruby-console-auto
;; See https://github.com/nonsequitur/inf-ruby
;;
;; Rinari (Ruby on Rails minor mode): http://rinari.rubyforge.org/
;;
;; rvm (Use rvm inside Emacs for managing ruby versions):
;; https://github.com/senny/rvm.el
;;
;; enhanced-ruby-mode (used instead of ruby-mode):
;; https://github.com/zenspider/enhanced-ruby-mode

;;; Code:
(defvar ruby-packages '(company-inf-ruby enh-ruby-mode inf-ruby rinari robe ruby-compilation ruby-mode rvm yaml-mode yari)
  "A list of packages to ensure are installed at launch.")

;; Defined in init.el
(ensure-package-installed ruby-packages)

(defun enable-gmbuell-ruby
    (interactive)
  (require 'enh-ruby-mode)
  (require 'robe)
  (require 'rinari)
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)

  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)

  (eval-after-load 'company
    '(progn
       (add-to-list 'company-backends 'company-inf-ruby)
       (add-to-list 'company-backends 'company-robe)))

  (defun ri-bind-key ()
    (define-key 'help-command "R" 'yari-helm))
  (add-hook 'ruby-mode-hook 'ri-bind-key)
  (add-hook 'enh-ruby-mode-hook 'ri-bind-key)

  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))

  (provide 'gmbuell-ruby))
;;; gmbuell-ruby.el ends here
