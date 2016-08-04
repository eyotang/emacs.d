(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))



(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "C-c b") 'sr-speedbar-toggle)

(provide 'init-sr-speedbar)
