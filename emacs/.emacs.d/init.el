;;; Package --- Summary
; Joshua Olson init.el
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(load-theme 'zenburn t)

(setq mac-option-modifier 'meta)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(desktop-save (quote ask-if-exists))
 '(desktop-save-mode t)
 '(direnv-mode t nil (direnv))
 '(flycheck-global-modes t)
 '(global-whitespace-cleanup-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (xref-js2 js2-refactor js2-mode haskell-mode gradle-mode groovy-mode sphinx-mode sphinx-doc flyspell-correct flymake-python-pyflakes flymake-haskell-multi magit zenburn-theme direnv flycheck jinja2-mode yaml-mode rainbow-mode rainbow-delimiters whitespace-cleanup-mode python-mode)))
 '(pyenv-mode t)
 '(show-trailing-whitespace t)
 '(whitespace-cleanup-mode-only-if-initially-clean nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
