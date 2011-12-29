(require 'package)

(defun install-and-initialize-packages (package-info)
  ;; Install all the packages
  (dolist (p package-info)
    (let ((package-name (car p)))
      (when (not (package-installed-p package-name)) (package-install package-name))
      )
    )

  ;; Ensure that everything under elpa is in the load path
  (let ((default-directory (concat dotfiles-dir "elpa")))
    (normal-top-level-add-subdirs-to-load-path))

  ;; Require and perform initialization for each package
  (dolist (p package-info)
    (let ((package-name (car p))
          (package-init (cdr p)))
      (require package-name)
      (funcall package-init)
      )
    )
  )

(provide 'util)
