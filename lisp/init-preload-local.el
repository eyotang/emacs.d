;; Get the view name
(setq view
      (let ((clearcase-root (getenv "CLEARCASE_ROOT")))
        (or (not clearcase-root)
            (and (string-match "/view/" clearcase-root)
                 (substring clearcase-root (match-end 0) nil))
            nil)))

(setq view (if (stringp view) view "NONE-VIEW"))
(defvar view-name view)

;; set desktop directory as "~/.emacs-desktop/<view>/"
(defvar desktop-dir (concat "~/.emacs-desktop/" view "/"))

(or (file-exists-p desktop-dir)
    (make-directory desktop-dir t)) ; Check and create desktop-dir



;;****************************************************************
;;                        MISC
;;****************************************************************
(global-set-key "\M-g" 'goto-line)  ; Bind goto-line with Alt+g
(global-set-key "\C-c\C-c" 'comment-region) ; Bind comment-region with Ctrl+c Ctrl+c
(global-set-key "\C-x\ c" 'revert-buffer)
(global-set-key "\C-x\ r\ p" 'replace-regexp) ; replace the marked-region


(require 'linum)                    ; Read linum.el
(global-linum-mode 1)               ; Display line number in left side


(provide 'init-preload-local)
