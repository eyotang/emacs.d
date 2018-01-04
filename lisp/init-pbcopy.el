(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))


(require 'pbcopy)
(turn-on-pbcopy)

(provide 'init-pbcopy)
