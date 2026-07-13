;;; init.el --- Joshua Olson's Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'package)

;; Keep installed packages / native-comp cache OUT of this (stow-linked) repo
;; directory so Emacs runtime state never pollutes config_stuff.
(setq package-user-dir (expand-file-name "~/.cache/emacs/elpa"))
(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/eln-cache/")))

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Packages this config expects. Keep in sync with `package-selected-packages'
;; in the custom block below. Installed on first run; failures warn but don't
;; abort init.
(defvar my/packages
  '(py-isort direnv flycheck flymake-haskell-multi flymake-python-pyflakes
    flyspell-correct haskell-mode jinja2-mode js2-mode js2-refactor magit
    python-mode rainbow-delimiters rainbow-mode sphinx-doc sphinx-mode
    whitespace-cleanup-mode xref-js2 yaml-mode zenburn-theme)
  "Packages required by this configuration.")

(unless (cl-every #'package-installed-p my/packages)
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (unless (package-installed-p pkg)
      (condition-case err
          (package-install pkg)
        (error (warn "init.el: failed to install %s: %s" pkg err))))))

;; macOS modifier keys + ligatures (emacs-mac / railwaycat build).
(setq mac-option-modifier 'meta)
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))
(when (window-system)
  (set-frame-font "Fira Code-14" t t))
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

;; Keep runtime state out of the (stow-linked) repo: backups/auto-saves under
;; ~/.saves, and Emacs bookkeeping (auto-save-list, desktop) under ~/.cache.
(let ((saves (expand-file-name "~/.saves/"))
      (cache (expand-file-name "~/.cache/emacs/")))
  (make-directory saves t)
  (make-directory cache t)
  (setq backup-directory-alist `(("." . ,saves))
        auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat saves "\\2") t))
        auto-save-list-file-prefix (concat cache "auto-save-list/.saves-")
        desktop-path (list cache)
        desktop-dirname cache))

;; Theme (guarded in case the install failed).
(when (package-installed-p 'zenburn-theme)
  (load-theme 'zenburn t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(provide 'init)
;;; init.el ends here
