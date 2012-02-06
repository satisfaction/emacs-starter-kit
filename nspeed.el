;; my favorite theme
(load-file "~/.emacs.d/vendor/Emacs-Sunburst-Color-Theme/color-theme-sunburst.el")
;;(require 'color-theme-sunburst)
(color-theme-sunburst)

;; use Deft
(require 'deft)
(setq deft-directory "~/Dropbox/nathan_speed/notes")

;; jshint
(add-to-list 'load-path "~/.emacs.d/vendor/node_modules/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook (lambda x() (flymake-mode t)))

;; Turns on flymake for all files which have a flymake mode
(add-hook 'find-file-hook 'flymake-find-file-hook)
