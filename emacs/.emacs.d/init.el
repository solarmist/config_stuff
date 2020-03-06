;;; Package --- Summary
;; Joshua Olson init.el

;; ############# Do not modify this section #############
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq mac-option-modifier 'meta)
;; ############# End Section #############

;; This is where nomral load/setup things can be added
(let ((default-directory  "~/.emacs.d/extern/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Manually installed from source.
;; git@github.com:emacs-lsp/lsp-python-ms.git
(add-to-list 'load-path "~/.emacs/extern/lsp-python-ms/")
;; git@github.com:emacsmirror/python-mode.git
(add-to-list 'load-path "~/.emacs/extern/python-mode/")


;; Explictly use python-mode.el that Barry maintains
(use-package python-mode
  :ensure t)

(load-theme 'zenburn t)
(set-frame-font "Fira Code")
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

(mac-auto-operator-composition-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(desktop-save-mode f)
 '(direnv-mode t nil (direnv))
 '(flycheck-global-modes t)
 '(global-whitespace-cleanup-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-log-io t)
 '(lsp-print-io t)
 '(package-selected-packages
   (quote
    (lsp-python-ms projectile company-lsp lsp-ui lsp-mode py-isort direnv flycheck flymake-haskell-multi flymake-python-pyflakes flyspell-correct gradle-mode groovy-mode haskell-mode jinja2-mode js2-mode js2-refactor magit python-mode rainbow-delimiters rainbow-mode sphinx-doc sphinx-mode whitespace-cleanup-mode xref-js2 yaml-mode zenburn-theme)))
 '(pyenv-mode t)
 '(show-trailing-whitespace t)
 '(whitespace-cleanup-mode-only-if-initially-clean (quote nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (require lsp-python-ms)
;; (add-hook 'python-mode 'lsp) ; or lsp-deferred

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :config

  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "~/other_projects/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "~/bin/Microsoft.Python.LanguageServer")
)
(use-package lsp-mode
;;   :ensure t
;;   ;; lsp-python-enable is created by macro above
;;   :hook (python-mode . lsp)
;;   :init
;;   (setq lsp-prefer-flymake nil)
  :config

  (setq lsp-enable-snippet nil)
;;   ;; change nil to 't to enable logging of packets between emacs and the LS
;;   ;; this was invaluable for debugging communication with the MS Python Language Server
;;   ;; and comparing this with what vs.code is doing
;;   (setq lsp-print-io nil))

;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp
;;   :ensure t
)

;; Only use the desktop-save-mode in a GUI
(when (window-system)
  (custom-set-variables
    '(desktop-save-mode t)
    '(desktop-save (quote ask-if-exists)))

;; Fix for using flyspell with Apple trackpad
(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))
