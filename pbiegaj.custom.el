;; baseline defaults
(load-file "~/.emacs.d/common.el")

;; set new twilight theme
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)

