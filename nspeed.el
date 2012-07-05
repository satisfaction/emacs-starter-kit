(require 'util)

(install-and-initialize-packages
 '(
   (deft . (lambda ()
             (setq deft-directory (if (string= system-name "switch.local")
                                         "~/Dropbox/gsfn/nathan_speed/notes/"
                                       "~/Dropbox/nathan_speed/notes/"))
             ))
   (pastels-on-dark-theme . (lambda() ))
   )
 )

(add-hook 'find-file-hook 'flymake-find-file-hook)
