(require 'util)

(install-and-initialize-packages
 '(
   (deft . (lambda ()
             (setq my-deft-directory (if (string= system-name "switch") "~/Dropbox/gsfn/nathan_speed/notes/" "~/Dropbox/nathan_speed/notes/"))
             (setq deft-directory my-deft-directory)
             ))
   (pastels-on-dark-theme . no-op)
   )
 )

(add-hook 'find-file-hook 'flymake-find-file-hook)

