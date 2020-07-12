;;; Package --- Summary
; Joshua Olson init.el
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq mac-option-modifier 'meta)
(mac-auto-operator-composition-mode)
(when (window-system)
  (set-frame-font "Fira Code-14"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; If setting up emacs comment out the following code for the time being
;; https://github.com/emacsmirror/python-mode for python-mode
(setq py-install-directory "/Users/solarmist/packages/python-mode") (add-to-list 'load-path py-install-directory) (require 'python-mode)

(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   (quote
    (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/Users/solarmist/.saves/\\2" t))))
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(column-number-mode t)
 '(desktop-save (quote ask-if-exists))
 '(desktop-save-mode t)
 '(direnv-mode t nil (direnv))
 '(flycheck-global-modes t)
 '(global-whitespace-cleanup-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/Business/Writing/2020-06-28-Summary.org")))
 '(package-selected-packages
   (quote
    (py-isort direnv flycheck flymake-haskell-multi flymake-python-pyflakes flyspell-correct haskell-mode jinja2-mode js2-mode js2-refactor magit python-mode rainbow-delimiters rainbow-mode sphinx-doc sphinx-mode whitespace-cleanup-mode xref-js2 yaml-mode zenburn-theme)))
 '(pyenv-mode t)
 '(show-trailing-whitespace t)
 '(whitespace-cleanup-mode-only-if-initially-clean t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
