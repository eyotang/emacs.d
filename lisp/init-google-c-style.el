(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4
        c-indent-level 4
        tab-width 4
        indent-tabs-mode nil
        default-tab-width 4))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'init-google-c-style)
