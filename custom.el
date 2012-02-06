;;;; Basic setup: colors, modifier keys, builtin behaviors, etc.
(setq kill-whole-line t)
(setq confirm-kill-emacs 'yes-or-no-p)
(delete-selection-mode)
(random t)                              ; reseed
(server-start)
(global-auto-revert-mode 1)
(set-cursor-color "black")

(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
;; on kinesis freestyle, 'Delete' sends kp-delete
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control kp-delete)] 'kill-word)

;; Mac-isms. They do no harm on non-macs.
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

;; while I <square box> Unicode as much as the next guy,
;; I want my lambdas left alone.
(remove-hook 'coding-hook 'pretty-lambdas)
(remove-hook 'coding-hook 'pretty-functions)

;; just nice to have everywhere; my screen is only so wide
(add-hook 'coding-hook (lambda () (setq tab-width 2)))

;; bar cursor makes it easier to see what delete-(backward-)char are going to hit
(add-to-list 'load-path "~/.emacs.d/vendor/bar-cursor")
(require 'bar-cursor)
(bar-cursor-mode 1)

;;;; line numbers on the left in a gui
(when window-system
  (add-to-list 'load-path "~/.emacs.d/vendor/linum")
  (require 'linum)
  (global-linum-mode 1))

;;;; Extra packages that the starter kit doesn't give us
(setq starter-kit-packages
      (append starter-kit-packages (list 'yasnippet-bundle
                                         'clojure-mode
                                         'paredit
                                         'haml-mode
                                         'sass-mode)))
(starter-kit-elpa-install)

(add-hook 'haml-mode-hook
          (lambda ()
            (run-hooks 'coding-hook)))

(add-to-list 'load-path "~/.emacs.d/vendor/el-expectations")
(require 'yaml-mode)

;;;; tab-completion configuration (hooray hippie-expand)
(setq hippie-expand-try-functions-list (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))
(global-set-key [(shift tab)] 'hippie-expand)
(global-set-key [(control tab)] 'hippie-expand)

;;;; TRAMP configuration
;; ssh is faster than scp for the small files I usually edit
;; remotely.
(setq tramp-default-method "ssh")

;;;; spiffy-mode provides a few utilities
(add-to-list 'load-path "~/.emacs.d/vendor/spiffy")
(setq spiffy-enable-minor-mode t)
(require 'spiffy)

;;;; spiffy-textmate-mode has some good stuff, but I don't want the
;;;; full minor mode since it stomps all over a bunch of default keybindings
(require 'spiffy-textmate-mode)
(global-set-key [(f5)] 'spiffy-tm-grep-project)
;; (add-hook 'coding-hook
;;           (lambda ()
;;             (local-set-key [f5] 'spiffy-tm-grep-project)
;;             (local-set-key [f8] 'spiffy-tm-open-file-in-project)
;;             (local-set-key [(control x) ?4 f8] 'spiffy-tm-open-file-in-project-other-window)))

;;;; configuration chunks too large to just jam in here
(require 'custom-ruby)
(require 'custom-color-compilation)

;;;; added cssh support
(add-to-list 'load-path "~/.emacs.d/vendor/cssh")
(require 'cssh)

;;;; added rvm support
(add-to-list 'load-path "~/.emacs.d/vendor/rvm")
(require 'rvm)

;;;; add autocomplete-mode
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete)

;;;; add autocomplete-etags
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-etags")
(require 'auto-complete-etags)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "tmp" "public/assets" "TAGS"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(push "/usr/local/bin" exec-path)
;; no more backup files emacs, I got it
(setq make-backup-files nil)

;; the default css indent level is 4; set it to 2
(setq css-indent-offset 2)

;; set font type and size
(set-default-font "Monaco-9")

;; rspec mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)
(add-to-list 'auto-mode-alist '("spec\\.rb$" . ruby-mode))
