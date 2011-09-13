(push "/usr/local/bin" exec-path)
;; no more backup files emacs, I got it
(setq make-backup-files nil)
;; set the javascript indent level to the non-standard 2 spaces
;; instead of 4
(setq-default js-indent-level 2)
;; set font type and size
(set-default-font "Monaco-9")
;; set new twilight theme
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)
;; C-n and C-p when the autocomplete menu is up
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-use-quick-help t)
;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)
;; ;; smart case
;; (setq ac-ignore-case 'smart)
;; rspec mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)
(add-to-list 'auto-mode-alist '("spec\\.rb$" . ruby-mode))

;; rinari minor mode
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)
