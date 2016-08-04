(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

(add-to-list 'ac-modes 'erlang-mode)

(setq erlang-electric-commands
  '(erlang-electric-comma
    erlang-electric-semicolon
    erlang-electric-gt
    erlang-electric-newline))           ; auto indent

(let ((default-directory  "~/.emacs.d/site-lisp"))
  (load "~/.emacs.d/site-lisp/subdirs.el" t t t))


(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "M-.") 'erl-find-source-under-point)
             ))
(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "M-,") 'erl-find-source-unwind)
             ))

(provide 'init-erlang)
