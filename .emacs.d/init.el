;; Process scripts and external libraries
;; Add all top-level subdirectories of .emacs.d to the load path
(add-to-list 'load-path "~/.emacs.d/")
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))

;; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(progn (cd "~/.emacs.d/extern")
       (normal-top-level-add-subdirs-to-load-path))

;; Requires
(require 'tramp)
(require 'xscheme)
;; External requires
(require 'color-theme-zenburn)

;; Set variables
(setq inhibit-splash-screen t)
(setq-default cperl-indent-level 4)
(setq debug-on-error t)
(setq make-backup-files nil)
(setq initial-frame-alist '((top . 10) (left . 30)
			    (width . 164) (height . 50)))
(setq default-frame-alist '((width . 85) (height . 50)))
(setq tramp-default-method "ssh")
(setq scheme-program-name
      "~/Local Applications/Development/mit-scheme.app/Contents/Resources/mit-scheme")

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|[tT]\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;; Setting to run right away
(progn (split-window-horizontally)
       (desktop-save-mode 1)
       )

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
