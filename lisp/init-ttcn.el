(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))

(require 'ttcn3)
(autoload 'ttcn-3-mode "ttcn-3-mode" "TTCN3 editing mode." t)
(setq auto-mode-alist (cons '("\\.ttcn" . ttcn-3-mode) auto-mode-alist))

(provide 'init-ttcn)
