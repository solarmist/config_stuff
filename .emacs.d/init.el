;; Joshua Olson init.el
;; Process scripts and external libraries

;; Add all top-level subdirectories of .emacs.d to the load path
(add-to-list 'load-path "~/.emacs.d/")
;; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|[tT]\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Set variables
(setq-default show-trailing-whitespace t)
(setq-default cperl-indent-level 4)
(setq inhibit-splash-screen t)
(setq debug-on-error t)
(setq make-backup-files nil)
(setq initial-frame-alist '((top . 10) (left . 30)
			    (width . 164) (height . 50)))
(setq default-frame-alist '((width . 85) (height . 50)))
(setq tramp-default-method "ssh")
(setq scheme-program-name
      "~/Local Applications/Development/mit-scheme.app/Contents/Resources/mit-scheme")

;; Autoload
(autoload 'js2-mode "js2" nil t)

;; Setting to run right away
(progn (cd "~/.emacs.d/extern")
       (normal-top-level-add-subdirs-to-load-path))
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))
(progn (split-window-horizontally)
       (desktop-save-mode 1)
       )

;; Requires
(require 'tramp)
(require 'xscheme)

;; External requires
(require 'color-theme-zenburn)
(require 'column-marker)

;; Hooks
(add-hook 'font-lock-mode-hook (lambda () (interactive) (column-marker-3 79)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Window system specific
(if window-system
    (progn (color-theme-zenburn)
	   ))

;; Set the initial working directory
(progn (cd "~/"))

;; Emacs controlled variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; (set-face-attribute 'default nil :height 180)
 )

;; Function for fixing mouse use mini-buffer errors
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
