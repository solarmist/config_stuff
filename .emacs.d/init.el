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
(require 'erc-services)

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

;; Load my passwords
(load "~/.emacs.d/.ercpass")
;; .ercpass should look like
;; (setq freenode-solarmist-pass "password")

 (setq erc-nickserv-passwords
       '((freenode (("solarmist" . ,freenode-solarmist-pass)))
	 ;; (DALnet (("nick" . "password")))
	 ))
;; Setup prompt to show channel name
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

;; Window system specific
(if window-system
    (progn (color-theme-zenburn)
	   (erc :server "irc.freenode.net"
		:port 6667
		:nick "solarmist"
		:password freenode-solarmist-pass
		;; :full-name
		)
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
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#emacs" "#screen" "#buildbot" "#erc"))))
 '(erc-autojoin-mode t)
 '(erc-interpret-mirc-color t)
 '(erc-modules (quote (autoaway completion match move-to-prompt ring services stamp spelling highlight-nicknames netsplit fill button match track readonly networks ring noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-nickserv-identify-mode (quote autodetect))
 '(erc-nickserv-passwords nil)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-services-mode t)
 '(global-font-lock-mode t)
 '(ispell-program-name "/opt/local/bin/ispell"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Function for fixing mouse use mini-buffer errors
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
