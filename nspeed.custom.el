;; baseline defaults
(load-file "~/.emacs.d/common.el")

;; my favorite theme
(load-file "~/.emacs.d/vendor/Emacs-Sunburst-Color-Theme/color-theme-sunburst.el")
;;(require 'color-theme-sunburst)
(color-theme-sunburst)

;; use Deft
(require 'deft)
(setq deft-directory "~/Dropbox/nathan_speed/notes")
