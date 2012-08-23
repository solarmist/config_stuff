;; Joshua Olson init.el
;; Process scripts and external libraries
;; Note: Modified Flyspell.el to use Mouse-3 rather than Mouse-2

;; Add all top-level subdirectories of .emacs.d to the load path
(add-to-list 'load-path "~/.emacs.d/")
;; Package managed libraries
(add-to-list 'load-path "~/.emacs.d/elpa")
;; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(let ((default-directory "~/.emacs.d/extern"))
	(normal-top-level-add-subdirs-to-load-path))

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
;; disable backup
(setq make-backup-files nil)
;; disable auto save
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
(if (not (daemonp))
    (progn (split-window-horizontally)
	   )
)

;; Requires
(require 'tramp)
(require 'xscheme)
(require 'erc)
(require 'erc-highlight-nicknames)

;; External requires
(require 'flymake-cursor)
(require 'color-theme-zenburn)
(require 'jinja2-mode)
(require 'rainbow-delimiters)


;; Load my passwords
(load "~/.emacs.d/.ercpass")
;; .ercpass should look like
;; (setq freenode-solarmist-pass "password")

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

;; these next 4 are to make buffer names moe useful wih same-name files
;; (like __init__.py)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ;; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ;; don't muck with special
;; buffers (or Gnus mail buffers)

;; (if (not window-system)
;;     (
;;      (defun tramp-list-tramp-buffers ()
;;        "Return a list of all Tramp connection buffers."
;;        (append
;;		(all-completions
;;		 "*tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))
;;		(all-completions
;;		 "*debug tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))))

;;      (defun tramp-list-remote-buffers ()
;;        "Return a list of all buffers with remote default-directory."
;;        (delq
;;		nil
;;		(mapcar
;;		 (lambda (x)
;;		   (with-current-buffer x
;;		     (when (and (stringp default-directory)
;;				(file-remote-p default-directory))
;;		       x)))
;;		 (buffer-list))))

;;      (defun flymake-create-temp-in-system-tempdir (filename prefix)
;;        (make-temp-file (or prefix "flymake")))

;;      ;; Flymake setup
;;      (setq flymake-log-level 3)
;;      (when (load "flymake" t)
;;        ;; Make sure it's not a remote buffer or flymake would not work
;;        (defun flymake-pylint-init ()
;; 	 (when (not (subsetp (list (current-buffer))
;; 			     (tramp-list-remote-buffers)))
;; 	   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 			      'flymake-create-temp-in-system-tempdir))
;; 		  (local-file (file-relative-name
;; 			       temp-file
;; 			       (file-name-directory buffer-file-name))))
;; 	     (list "~/bin/pychecker.sh" (list temp-file)))))

;;        (add-to-list 'flymake-allowed-file-name-masks
;; 		    '("*\\.py" flymake-pylint-init)))
;;      (add-hook 'find-file-hook 'flymake-find-file-hook)
;;      )
;; )
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
(if (or (string-match
	 (substring emacs-version 0 2)
	 "23")
	(string-match
	 (substring emacs-version 0 2)
	 "24"))
    (progn (color-theme-zenburn)
       ))

(if window-system
    (progn (desktop-save-mode 1)
	   (setq ispell-program-name "/opt/local/bin/ispell")
     )
)

;; Change default font for work machine
(if (and window-system (string= "JOSOIMAC27A.local" system-name))
    (progn (set-face-attribute 'default nil :height 180)
	  (erc :server "irc.friendfinderinc.com"
	       :port 7000
	       :nick "jdolson"
	       :password ffn-jdolson-pass)
	   (erc :server "irc.freenode.net"
		:port 6667
		:nick "jdolson"
		)
	  )
)

(if (and window-system (string= "Haruhi" (substring system-name 0 6)))
    (progn (set-face-attribute 'default nil :height 180)
	   (erc :server "irc.freenode.net"
		   :port 6667
		   :nick "solarmist"
		   :password freenode-solarmist-pass
		   )
	   (erc :server "irc.mitx.mit.edu"
		   :port 6667
		   :nick "solarmist"
		   )
	   (desktop-save-mode 1)
	   )
)


;; Emacs controlled variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#erc" "#screen" "#buildbot" "#emacs") ("irc.friendfinderinc.com" "#builds" "#prog") ("irc.mitx.mit.edu" "#6002"))))
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
 '(whitespace-line-column 79)
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
