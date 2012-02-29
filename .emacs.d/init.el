;; Joshua Olson init.el
;; Process scripts and external libraries
;; Note: Modified Flyspell.el to use Mouse-3 rather than Mouse-2

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
(setq-default cperl-indent-level 4)
(setq inhibit-splash-screen t)
(setq debug-on-error t)
; disable backup
(setq make-backup-files nil)
; disable auto save
(setq auto-save-default nil)
(setq initial-frame-alist '((top . 10) (left . 30)
			    (width . 165) (height . 50)))
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
(require 'jinja2-mode)
(require 'rainbow-delimiters)

(load "~/.emacs.d/extern/flymake-cursor.el")

;; Load my passwords
(load "~/.emacs.d/.ercpass")
;; .ercpass should look like
;; (setq freenode-solarmist-pass "password")

;; Hooks
;; Hooks for programming modes
(dolist (hook (list 'c-mode-common-hook
		    'emacs-lisp-mode-hook
		    'scheme-mode-hoook
		    'python-mode-hook
		    'lisp-mode-hook
		    'jinja2-mode
		    'css-mode
		    'html-mode
		    ))
  (add-hook hook 'rainbow-delimiters-mode)
  (add-hook hook 'whitespace-mode)
  (add-hook hook 'flyspell-prog-mode)
  )

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Flymake setup
(setq flymake-log-level 3)
(when (load "flymake" t)
  ; Make sure it's not a remote buffer or flymake would not work
  (defun flymake-pyflakes-init ()
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-in-system-tempdir))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "~/bin/pychecker.sh" (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Erc setup
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

;; Needs 23+
(if (or (string-match emacs-version "23") (string-match emacs-version "24"))
    (progn (color-theme-zenburn)
       ))

;; Set the initial working directory
(progn (cd "~/"))

;; Change default font for work machine
(if (string= "JOSOIMAC27A.local" system-name)
    (progn (set-face-attribute 'default nil :height 180)
	  ;; (erc :server "irc.friendfinderinc.com"
	  ;;      :port 7000
	  ;;      :nick "jdolson"
	  ;;      :password ffn-jdolson-pass)
	  ;;  (erc :server "irc.freenode.net"
	  ;;	:port 6667
	  ;;	:nick "jdolson"
	  ;;	;; :full-name
	  ;;	)
	  )
)

(if (and window-system (string= "Haruhi.local" system-name))
    (progn (erc :server "irc.freenode.net"
		:port 6667
		:nick "solarmist"
		:password freenode-solarmist-pass
		;; :full-name
		)
	   (set-face-attribute 'default nil :height 180)
	   )
)


;; Emacs controlled variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#erc" "#screen" "#buildbot" "#emacs") ("irc.friendfinderinc.com" "#builds" "#prog"))))
 '(erc-autojoin-mode t)
 '(erc-interpret-mirc-color nil)
 '(erc-mode-hook (quote (pcomplete-erc-setup erc-munge-invisibility-spec erc-move-to-prompt-setup erc-button-setup erc-imenu-setup erc-spelling-mode)))
 '(erc-modules (quote (completion highlight-nicknames netsplit fill button match track readonly networks ring noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-nickserv-identify-mode (quote autodetect))
 '(erc-nickserv-passwords nil)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-services-mode t)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/Library/Frameworks/Python.framework/Versions/2.7/bin" "/usr/local/bin")))
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ispell-program-name "/opt/local/bin/ispell")
 '(whitespace-style (quote (face trailing lines-tail space-before-tab empty space-after-tab))))
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
