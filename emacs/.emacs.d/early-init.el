;;; early-init.el --- Runs before package-initialize  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Emacs 27+ calls `package-initialize' automatically *before* loading init.el,
;; using the default package-user-dir (~/.emacs.d/elpa). Because ~/.emacs.d is a
;; stow symlink into the config_stuff repo, that would write packages into the
;; repo. Redirect package + native-comp state OUT of the repo here, early enough
;; to take effect before the automatic initialize.
(setq package-user-dir (expand-file-name "~/.cache/emacs/elpa"))
(when (and (fboundp 'startup-redirect-eln-cache)
           (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/eln-cache/")))

;;; early-init.el ends here
