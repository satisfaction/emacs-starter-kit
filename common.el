(push "/usr/local/bin" exec-path)
;; no more backup files emacs, I got it
(setq make-backup-files nil)

;; the default css indent level is 4; set it to 2
(setq css-indent-offset 2)

;; set font type and size
(set-default-font "Monaco-9")

;; rspec mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)
(add-to-list 'auto-mode-alist '("spec\\.rb$" . ruby-mode))
