(setq scheme-program-name
            "/Applications/Development/mit-scheme.app/Contents/Resources/mit-scheme")
(require 'xscheme)

(setq inhibit-splash-screen t)
(setq debug-on-error t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; Congratulations! You're customizing your editor
(add-to-list 'load-path "~/.emacs.d/")

; Add all top-level subdirectories of .emacs.d to the load path
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))
; Third party libraries are stored in ~/.emacs.d/extern
(add-to-list 'load-path "~/.emacs.d/extern")
(progn (cd "~/.emacs.d/extern")
       (normal-top-level-add-subdirs-to-load-path))

(require 'color-theme-zenburn)

(color-theme-zenburn)

(progn (cd "~/Documents/"))
