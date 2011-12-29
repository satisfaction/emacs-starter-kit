(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(require 'util)

;; Emacs has a built-in feature for configuring the emacs environment with a psuedo gui
;; instead of via config files like this one. It's called Custom or "customizations" and
;; it is horrible. Here we cause Custom's noise not to pollute our init.el:
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'no-error)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; List of (package . (lambda() setup))s to be installed for everyone
(defun no-op () ())
(install-and-initialize-packages
 '(
   (starter-kit . no-op)
   (starter-kit-lisp . no-op)
   (starter-kit-ruby . no-op)
   (starter-kit-js . no-op)
   (flymake . no-op)
   (flymake-ruby . no-op)
   (flymake-jshint . (lambda () (add-hook 'javascript-mode-hook (lambda () (flymake-mode t)))))
   (rainbow-mode . no-op)
   (bm . no-op)
;; rspec-mode is not currently compiling (rspec-mode-1.3 from elpa on emacs 24)
;; (rspec-mode . (lambda ()
;;                   (require 'rspec-mode)
;;                   (add-to-list 'auto-mode-alist '("spec\\.rb$" . ruby-mode))
;;                   ))
;;   )
   )
 )

;; auto update of modified files from the filesystem, mostly for
;; sharing two different editors on the same machine
(global-auto-revert-mode t)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

;; 4 Macs
(setq mac-command-modifier 'meta)

;; not sure exactly what this is for, but seems generally useful
(push "/usr/local/bin" exec-path)

;; no more backup files emacs, I got it
(setq make-backup-files nil)

;; set font type and size
(set-default-font "Monaco-9")

;; css indent
(setq css-indent-offset 2)


