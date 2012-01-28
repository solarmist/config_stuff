;; Joshua Olson init.el
;; Process scripts and external libraries

;; Add all top-level subdirectories of .emacs.d to the load path
(add-to-list 'load-path "~/.emacs.d/")
;; Package managed libraries
(add-to-list 'load-path "~/.emacs.d/elpa")
;; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist
	     '("\\.\\([pP][Llm]\\|al\\|[tT]\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Set variables
(setq dired-listing-switches "-lRS")
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
      "~/Local Applications/Development/mit-scheme.app/\
Contents/Resources/mit-scheme")

;; Autoload
(autoload 'js2-mode "js2" nil t)

(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)

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
(require 'erc)
(require 'erc-highlight-nicknames)

;; External requires
(require 'color-theme-zenburn)
(require 'column-marker)
(require 'jinja2-mode)

;; Hooks
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
(add-hook 'font-lock-mode-hook (lambda () (interactive) (column-marker-3 79)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Erc setup
;; Set the irc channels to join
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#screen" "#buildbot" "#erc")
	;; Other servers and rooms
	))

(setq erc-interpret-mirc-color t)

(setq erc-prompt (lambda ()
     (if (and (boundp 'erc-default-recipients)
	      (erc-default-target))
         (erc-propertize (concat (erc-default-target) ">")
			 'read-only t
			 'rear-nonsticky t
			 'front-nonsticky t)
       (erc-propertize (concat "ERC>")
		       'read-only t
		       'rear-nonsticky t
		       'front-nonsticky t))))
(and
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

(erc :server "irc.freenode.net"
     :port 6667
     :nick "solarmist"
     ;; :password
     ;; :full-name
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
 '(column-number-mode t)
 '(global-font-lock-mode t))
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
