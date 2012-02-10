(require 'util)

(install-and-initialize-packages
 '(
   (deft . (lambda () (setq deft-directory "~/Dropbox/nathan_speed/notes/")))
   (pastels-on-dark-theme . no-op)
   )
 )

(add-hook 'find-file-hook 'flymake-find-file-hook)
