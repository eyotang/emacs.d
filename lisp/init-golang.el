(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))

(require 'go-mode-autoloads)

(defun style-hook ()
  (setq c-basic-offset 4
        c-indent-level 4
        tab-width 4
        indent-tabs-mode nil
        default-tab-width 4))

(add-hook 'go-mode-hook 'style-hook)

(provide 'init-golang)
