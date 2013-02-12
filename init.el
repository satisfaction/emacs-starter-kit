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
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			  ("marmalade" . "http://marmalade-repo.org/packages/")
			  ("josh" . "http://josh.github.com/elpa")
			  ("gnu" . "http://elpa.gnu.org/packages")))
;;;Add the following custom-set-variables to use 'lazy' modes
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js3-lazy-commas t)
 '(js3-lazy-operators t)
 '(js3-lazy-dots t)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-curly-indent-offset 2)
)

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.hamljs$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

;; List of (package . (lambda() setup))s to be installed for everyone
(defun no-op () ())
(install-and-initialize-packages
 '(
   (starter-kit . no-op)
   (starter-kit-lisp . no-op)
   (starter-kit-ruby
    . (lambda() (setq ruby-deep-indent-paren nil))) ;; normal indentation for ruby-mode
   (starter-kit-js
    . (lambda()
        (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
        (add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))
        (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))))
   (starter-kit-js
    . (lambda()
        (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
        (add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))
        (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))))
   (flymake . no-op)
   (flymake-ruby . no-op)
   (flymake-jshint
    . (lambda ()
        (add-hook 'javascript-mode-hook (lambda () (flymake-mode t)))
        (defun flymake-display-err-message-for-current-line ()
          "Display a message with errors/warnings for current line if it has errors and/or warnings."
          (interactive)
          (let* ((line-no             (flymake-current-line-no))
                 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info 
                                                                    line-no)))
                 (menu-data           (flymake-make-err-menu-data line-no 
                                                                  line-err-info-list)))
            (if menu-data
                (let ((messages))
                  (push (concat (car menu-data) ":") messages)
                  (dolist (error-or-warning (cadr menu-data))
                    (push (car error-or-warning) messages))
                  (message "%s" (mapconcat #'identity (reverse messages) "\n"))))))

        (global-set-key (kbd "C-x q") 'flymake-display-err-message-for-current-line)
        ))
   (bm . no-op)
   (haml-mode . no-op)
   (smart-tab . (lambda () (global-smart-tab-mode 1)))
;; rspec-mode is not currently compiling (rspec-mode-1.3 from elpa on emacs 24)
;; (rspec-mode . (lambda ()
;;                   (require 'rspec-mode)
;;                   (add-to-list 'auto-mode-alist '("spec\\.rb$" . ruby-mode))
;;                   ))
;;   )
   )
 )
(setq-default coffee-tab-width 2)
;; auto update of modified files from the filesystem, mostly for
;; sharing two different editors on the same machine
(global-auto-revert-mode t)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

;; 4 Macs
(setq w32-lwindow-modifier 'meta)

;; copy and paste from terminal emacs in OS X (see https://gist.github.com/267162)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; not sure exactly what this is for, but seems generally useful
(push "/usr/local/bin" exec-path)

;; no more backup files emacs, I got it
(setq make-backup-files nil)

;; css indent
(setq css-indent-offset 4)

;; javascript indent
(setq js-indent-level 4)

;; disable auto-fill-mode in text files
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; remove paredit minor mode from javascript mode
(remove-hook 'js-mode-hook 'esk-paredit-nonlisp)
;; disable paredit-mode in every file
(add-hook 'find-file-hook (lambda () (paredit-mode -1)))
(require 'ess-site)
(load-theme 'wombat)

(defface hl-line '((t (:background "DarkSlateGrey")))
 "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) 
