(push "/usr/local/bin" exec-path)
;; no more backup files emacs, I got it
(setq make-backup-files nil)
;; set the javascript indent level to the non-standard 2 spaces
;; instead of 4
(setq-default js-indent-level 2)
;; set font type and size
(set-default-font "Monaco-9")
;; set new twilight theme
(color-theme-twilight)
;; C-n and C-p when the autocomplete menu is up
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; smart case
(setq ac-ignore-case 'smart)
