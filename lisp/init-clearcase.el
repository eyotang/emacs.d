;;  "Add all subdirectories of current directory to `load-path'.
;; More precisely, this uses only the subdirectories whose names
;; start with letters or digits;"
(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))

(require 'clearcase)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(clearcase-checkin-arguments (quote ("-nc")))
 '(clearcase-checkout-arguments (quote ("-unreserve")))
 '(clearcase-diff-on-checkin nil)
 '(clearcase-keep-uncheckouts nil)
 '(clearcase-suppress-checkout-comments t)
 '(clearcase-suppress-confirm nil))



(provide 'init-clearcase)
