;;; vc-cc-hooks.el --- support for vc-cc.el, formerly resident

;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: ttn@netcom.com
;; Version: 5.3 + CVS hacks by ceder@lysator.liu.se made in Jan-Feb 1994.
;;
;; XEmacs fixes, CVS fixes, and general improvements
;; by Jonathan Stigelman <Stig@hackvan.com>

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.28.

;;; Commentary:

;; This is like vc-hooks.el, but for vc-cc.  See the commentary of
;; vc-hooks.el and vc-cc.el.

;;; Code:

(eval-and-compile
  (require 'advice))

;; Using defconst only because we may have already loaded another version of
;; this library!
(defconst vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS)
    vc-cc-registered
    vc-find-cvs-master)
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defconst ClearCase "@@")

(defvar vc-path
  (if (file-exists-p "/usr/sccs")
      '("/usr/sccs") nil)
  "*List of extra directories to search for version control commands.")

(defvar vc-make-backup-files nil
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups.")

(defvar vc-follow-symlinks 'ask
  "*Indicates what to do if you visit a symbolic link to a file
that is under version control.  Editing such a file through the
link bypasses the version control system, which is dangerous and
probably not what you want.
  If this variable is t, VC follows the link and visits the real file,
telling you about it in the echo area.  If it is `ask', VC asks for
confirmation whether it should follow the link.  If nil, the link is
visited and a warning displayed.")

(defvar vc-display-status t
  "*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed.")

(defvar vc-cc-display-branch t
  "*If non-nil, full branch name of ClearCase working file displayed in modeline.
Otherwise, just the version number or label is displayed.")

(defvar vc-auto-dired-mode t
  "*If non-nil, automatically enter `vc-dired-mode' in dired-mode buffers where
version control is set-up.")

(defvar vc-cc-pwv nil ;; (getenv "CLEARCASE_ROOT")
  "The ClearCase present working view for the current buffer.")
(make-variable-buffer-local 'vc-cc-pwv)

(defvar vc-consult-headers t
  "*If non-nil, identify work files by searching for version headers.")

(defconst vc-elucidated (string-match "Lucid" emacs-version))

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode vc-mode)
                                 minor-mode-alist)))
;; We don't really need to have the toggling feature provided by this command,
;; so in deference to FSF Emacs, I won't use it.
;;(add-minor-mode 'vc-mode 'vc-mode)

(defvar vc-mode nil)                    ; used for modeline flag
(make-variable-buffer-local 'vc-mode)
(set-default 'vc-mode nil)
(put 'vc-mode 'permanent-local t)

(defvar vc-dired-mode nil)
(make-variable-buffer-local 'vc-dired-mode)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we don't
;; want to recompute it even on every find.

(defmacro vc-error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defvar vc-file-prop-obarray (make-vector 17 0)
  "Obarray for per-file properties.")

(defun vc-file-setprop (file property value)
  ;; set per-file property
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  ;; get per-file property
  (get (intern file vc-file-prop-obarray) property))

(defun vc-file-clearprops (file)
  ;; clear all properties of a given file
  (setplist (intern file vc-file-prop-obarray) nil))

;;; actual version-control code starts here

(defun vc-registered (file)
  (let (handler)
    (if (boundp 'file-name-handler-alist)
        (setq handler (find-file-name-handler file 'vc-registered)))
    (if handler
        (funcall handler 'vc-registered file)
      ;; Search for a master corresponding to the given file
      (let ((dirname (or (file-name-directory file) ""))
            (basename (file-name-nondirectory file)))
        (catch 'found
          (mapcar
           (function
            (lambda (s)
              (if (atom s)
                  (funcall s dirname basename)
                (let ((trial (format (car s) dirname basename)))
                  (if (and (file-exists-p trial)
			   ;; This ensures that directories are not considered
			   ;; to be registered files (this happens with the
			   ;; third RCS pattern in vc-master-templates).
			   (not (equal basename ""))
                           ;; Make sure the file we found with name
                           ;; TRIAL is not the source file itself.
                           ;; That can happen with RCS-style names
                           ;; if the file name is truncated
                           ;; (e.g. to 14 chars).  See if either
                           ;; directory or attributes differ.
                           (or (not (string= dirname
                                             (file-name-directory trial)))
                               (not (equal
                                     (file-attributes file)
                                     (file-attributes trial)))))
                      (throw 'found (cons trial (cdr s))))))))
           vc-master-templates)
          nil)))))

(defun vc-cc-registered (dirname basename)
  ;; Check if DIRNAME/BASENAME is a ClearCase element
  ;; If it is, do a (throw 'found (cons MASTER '@@)).
  ;; Use general purpose function for real check
  ;; This should only be used in vc-master-template.
  (let ((fullname (concat dirname basename)))
    ;; If this is a symlink to a ClearCase file, it will think that it is
    ;; under control, but won't be able to get all information with
    ;; vc-fetch-properties.  We should leave it up to the user to chase the
    ;; link, or simply not edit the file through the link.
    (if (and (not (file-symlink-p fullname))
             (clearcase-element-p fullname))
        (throw 'found (cons fullname '@@))))
  )

(defun vc-find-cvs-master (dirname basename)
  ;; Check if DIRNAME/BASENAME is handled by CVS.
  ;; If it is, do a (throw 'found (cons MASTER 'CVS)).
  ;; Note: If the file is ``cvs add''ed but not yet ``cvs commit''ed
  ;; the MASTER will not actually exist yet.  The other parts of VC
  ;; checks for this condition.  This function returns something random if
  ;; DIRNAME/BASENAME is not handled by CVS.
  ;; This should only be used in vc-master-template.
  (and (string= "" dirname) (setq dirname default-directory))
  (if (and (file-directory-p (concat dirname "CVS/"))
           (file-readable-p (concat dirname "CVS/Entries")))
      (let ((fname (concat dirname basename))
            sbuf rev)
        (unwind-protect
            (save-excursion
              (set-buffer (generate-new-buffer " vc-scratch"))
              (setq sbuf (current-buffer))
              (insert-file-contents (concat dirname "CVS/Entries"))
              (cond
               ((re-search-forward
                 (concat "^/" (regexp-quote basename) "/\\([0-9.]*\\)/.*/\\(T\\([^/\n]+\\)\\)?$")
                 nil t)
                ;; We found it.  Store version number, and branch tag
                (setq rev (buffer-substring (match-beginning 1)
                                            (match-end 1)))
                (vc-file-setprop fname 'vc-your-latest-version rev)
                ;; XEmacs - we put something useful in the modeline
                (vc-file-setprop fname 'sticky-tag
                                 (cond ((string= "0" rev) "newfile")
                                       ((match-beginning 3)
                                        (buffer-substring (match-beginning 3)
                                                          (match-end 3)))
                                       (t "main")))
                (erase-buffer)
                (insert-file-contents (concat dirname "CVS/Repository"))
                (let ((master
                       (concat (file-name-as-directory
                                (buffer-substring (point-min)
                                                  (1- (point-max))))
                               basename
                               ",v")))
                  (throw 'found (cons master 'CVS))))))
          (kill-buffer sbuf)))))

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered."
  (or (vc-file-getprop file 'vc-name)
      (let ((name-and-type (vc-registered file)))
        (if name-and-type
            (progn
              (vc-file-setprop file 'vc-backend (cdr name-and-type))
              (vc-file-setprop file 'vc-name (car name-and-type)))))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered."
  (and file
       (or (vc-file-getprop file 'vc-backend)
           (let ((name-and-type (vc-registered file)))
             (if name-and-type
                 (progn
                   (vc-file-setprop file 'vc-name (car name-and-type))
                   (vc-file-setprop file 'vc-backend (cdr name-and-type))))))))

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with a form of version control
that locks files by making them read-only (i.e.: not CVS), then check the
file in or out.  Otherwise, just change the read-only flag of the buffer.

If you provide a prefix argument, we pass it on to `vc-next-action'."
  (interactive "P")
  (let ((vc-type (vc-backend-deduce (buffer-file-name))))
    (cond ((and vc-type
                buffer-read-only
                (file-writable-p buffer-file-name)
                (/= 0 (user-uid)))
           ;; XEmacs - The buffer isn't read-only because it's locked, so
           ;; keep vc out of this...
           (toggle-read-only))
          ((and vc-type (not (eq 'CVS  vc-type)))
           (vc-next-action verbose))
          (t
           (toggle-read-only)))
    ))

;; Map the vc-toggle-read-only key whereever toggle-read-only was
(let ((where (where-is-internal 'toggle-read-only global-map)))
  (if where
      (mapcar (lambda (key)
                (define-key global-map
                  key 'vc-toggle-read-only))
              where))
  )
;;(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

;; For other cases, try advising...
(defadvice toggle-read-only (around vc activate)
  "If file is under version control, perform `vc-next-action'."
  (if vc-mode
      (let ((vc-type (vc-backend-deduce (buffer-file-name))))
        (cond ((and vc-type
                    buffer-read-only
                    (file-writable-p buffer-file-name)
                    (/= 0 (user-uid)))
               ;; XEmacs - The buffer isn't read-only because it's locked, so
               ;; keep vc out of this...
               ad-do-it)
              ((and vc-type (not (eq 'CVS  vc-type)))
               (vc-next-action (ad-get-arg 0)))
              (t ad-do-it))
        )
    ad-do-it
    ))

(defun vc-file-owner (file)
  ;; XEmacs - vc-locking-user is just WAY too slow.
  (let* ((fa (file-attributes file)))
    (cond ((eq ?w (aref (nth 8 fa) 2))  ; -rw-r--r--
           ;; #### - if it's writable, we trust unix...dumb move?
           (user-login-name (nth 2 fa)))
          (t
           ;; big slowness here...
           (require 'vc)
           (vc-locking-user file)
           ))))

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name."
  (interactive (list buffer-file-name nil))
  (if file
      (let ((vc-type (vc-backend-deduce file)))
        (setq vc-mode
              (if vc-type
                  (concat " " (or label (symbol-name vc-type))
                          (if vc-display-status
                              (vc-status file vc-type)))))
        ;; Even root shouldn't modify a registered file without
        ;; locking it first.
        (and vc-type
             (not (string= (user-login-name) (vc-file-owner file)))
             (setq buffer-read-only t))
        (and (null vc-type)
             (file-symlink-p file)
             (let ((link-type (vc-backend-deduce (file-symlink-p file))))
               (if link-type
                   (message
                    "Warning: symbolic link to %s-controlled source file"
                    link-type))))
        (redraw-modeline)
        ;;(set-buffer-modified-p (buffer-modified-p))  ;;use this if Emacs 18
        vc-type)))

(defun vc-status (file vc-type)
  ;; Return string for placement in modeline by `vc-mode-line'.
  ;; If FILE is not registered, return nil.
  ;; If FILE is registered but not locked, return " REV" if there is a head
  ;; revision and " @@" otherwise.
  ;; If FILE is locked then return all locks in a string of the
  ;; form " LOCKER1:REV1 LOCKER2:REV2 ...", where "LOCKERi:" is empty if you
  ;; are the locker, and otherwise is the name of the locker followed by ":".

  ;; Algorithm:

  ;; Check for master file corresponding to FILE being visited.
  ;;
  ;; RCS: Insert the first few characters of the master file into a
  ;; work buffer.  Search work buffer for "locks...;" phrase; if not
  ;; found, then keep inserting more characters until the phrase is
  ;; found.  Extract the locks, and remove control characters
  ;; separating them, like newlines; the string " user1:revision1
  ;; user2:revision2 ..." is returned.
  ;;
  ;; SCCS: Check if the p-file exists.  If it does, read it and
  ;; extract the locks, giving them the right format.  Else use prs to
  ;; find the revision number.
  ;;
  ;; CVS: vc-find-cvs-master has already stored the current revision
  ;; number and sticky-tag for the file.  XEmacs displays the sticky-tag.

  ;; Limitations:

  ;; The output doesn't show which version you are actually looking at.
  ;; The modeline can get quite cluttered when there are multiple locks.
  ;; The head revision is probably not what you want if you've used `rcs -b'.

  (let ((master (vc-name file))
        found
        status)

    ;; If master file exists, then parse its contents, otherwise we
    ;; return the nil value of this if form.
    (if (and master vc-type)
        (save-excursion

          ;; Create work buffer.
          (set-buffer (get-buffer-create " *vc-status*"))
          (setq buffer-read-only nil
                default-directory (file-name-directory master))
          (erase-buffer)

          ;; Set the `status' var to the return value.
          (cond

           ;; RCS code.
           ((eq vc-type 'RCS)
            ;; Check if we have enough of the header.
            ;; If not, then keep including more.
            (while
                (not (or found
                         (let ((s (buffer-size)))
                           (goto-char (1+ s))
                           (zerop (car (cdr (insert-file-contents
                                             master nil s (+ s 8192))))))))
              (beginning-of-line)
              (setq found (re-search-forward "^locks\\([^;]*\\);" nil t)))

            (if found
                ;; Clean control characters and self-locks from text.
                (let* ((lock-pattern
                        (concat "[ \b\t\n\v\f\r]+\\("
                                (regexp-quote (user-login-name))
                                ":\\)?"))
                       (locks
                        (save-restriction
                          (narrow-to-region (match-beginning 1) (match-end 1))
                          (goto-char (point-min))
                          (while (re-search-forward lock-pattern nil t)
                            (replace-match (if (eobp) "" ":") t t))
                          (buffer-string))))
                  (setq status
                        (if (not (string-equal locks ""))
                            locks
                          (goto-char (point-min))
                          (if (looking-at "head[ \b\t\n\v\f\r]+\\([.0-9]+\\)")
                              (concat "-"
                                      (buffer-substring (match-beginning 1)
                                                        (match-end 1)))
                            " @@"))))))

           ;; SCCS code.
           ((eq vc-type 'SCCS)
            ;; Build the name of the p-file and put it in the work buffer.
            (insert master)
            (search-backward "/s.")
            (delete-char 2)
            (insert "/p")
            (if (not (file-exists-p (buffer-string)))
                ;; No lock.
                (let ((exec-path (if (boundp 'vc-path) (append exec-path vc-path)
                                   exec-path)))
                  (erase-buffer)
                  (insert "-")
                  (if (zerop (call-process "prs" nil t nil "-d:I:" master))
                      (setq status (buffer-substring 1 (1- (point-max))))))
              ;; Locks exist.
              (insert-file-contents (buffer-string) nil nil nil t)
              (while (looking-at "[^ ]+ \\([^ ]+\\) \\([^ ]+\\).*\n")
                (replace-match " \\2:\\1"))
              (setq status (buffer-string))
              (aset status 0 ?:)))
           ;; CVS code.
           ((eq vc-type 'CVS)
            ;; sticky-tag is initialized by vc-backend-deduce
            (setq status (concat ":" (vc-file-getprop file 'sticky-tag) "-"
                                 (vc-file-getprop file 'vc-your-latest-version)
                                 ))
            )
           ;; ClearCase code.
           ((eq vc-type '@@)
            (require 'vc)
            ;; Display the explicitly specified version or the latest version
            (let ((version (or (vc-cc-version-name file)
                               (vc-latest-version file)
                               ;; Make sure version is a string in case the
                               ;; file is not really a versioned object
                               "")))
              ;; Check if the user wants to see the branch
              (if vc-cc-display-branch
                  (setq status version)
                (setq status (concat ":" (file-name-nondirectory version))))
              ))
           )

          ;; Clean work buffer.
          (erase-buffer)
          (set-buffer-modified-p nil)
          status))))

;;;;; install a call to the above as a find-file hook

(defun vc-follow-link ()
  ;; If the current buffer visits a symbolic link, this function makes it
  ;; visit the real file instead.  If the real file is already visited in
  ;; another buffer, make that buffer current, and kill the buffer
  ;; that visits the link.
  (let* ((truename (abbreviate-file-name (file-chase-links buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
	 (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
	(progn
	  (kill-buffer this-buffer)
	  ;; In principle, we could do something like set-visited-file-name.
	  ;; However, it can't be exactly the same as set-visited-file-name.
	  ;; I'm not going to work out the details right now. -- rms.
	  (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

(defun vc-set-auto-mode ()
  "Check again for the mode of the current buffer when using ClearCase version extended paths."

  (if (eq (vc-file-getprop buffer-file-name 'vc-backend) '@@)
      (let* ((version (vc-cc-version-name buffer-file-name))
             (buffer-file-name (vc-cc-element-name buffer-file-name)))
        ;; Need to recheck the major mode only if a version was appended
        (if version (set-auto-mode))
        ;; Set a buffer-local variable for the working view
        (setq vc-cc-pwv (vc-cc-pwv buffer-file-name))
        ))
  )

(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (cond
   (buffer-file-name
    (vc-file-clearprops buffer-file-name)
    (cond
     ((vc-backend-deduce buffer-file-name)
      (vc-mode-line buffer-file-name)
      (cond ((not vc-make-backup-files)
	     ;; Use this variable, not make-backup-files,
	     ;; because this is for things that depend on the file name.
	     (make-local-variable 'backup-inhibited)
	     (setq backup-inhibited t)))
      (vc-set-auto-mode))
     ((let* ((link (file-symlink-p buffer-file-name))
	     (link-type (and link (vc-backend-deduce (file-chase-links link)))))
	(if link-type
            (cond ((null vc-follow-symlinks)
                   (message
                    "Warning: symbolic link to %s-controlled source file" link-type))
                  ((or (not (eq vc-follow-symlinks 'ask))
		       ;; If we already visited this file by following
		       ;; the link, don't ask again if we try to visit
		       ;; it again.  GUD does that, and repeated questions
		       ;; are painful.
                       (let ((find-file-compare-truenames nil))
                         ;; If compare-truenames is t, this will always be t
                         (get-file-buffer
                          (abbreviate-file-name (file-chase-links buffer-file-name)))))

		   (vc-follow-link)
		   (message "Followed link to %s" buffer-file-name)
		   (vc-find-file-hook))
                  (t
                   (if (yes-or-no-p
                        (format
                         "Symbolic link to %s-controlled source file; follow link? "
                         link-type))
                       (progn (vc-follow-link)
                              (message "Followed link to %s" buffer-file-name)
                              (vc-find-file-hook))
                     (message
                      "Warning: editing through the link bypasses version control")
                     ))))
        (vc-set-auto-mode)))
      ))))

;;; install a call to the above as a find-file hook
(add-hook 'find-file-hooks 'vc-find-file-hook)

;; Handle ClearCase version files correctly.
;;
;; This little bit of magic causes the buffer name to be set to
;; <filename>@@/<branch path>/<version>, if you find a specific version of
;; a file.  Without this the name of the buffer will just be the version
;; number.

(defun vc-check-cc-name ()
  (let ((match (string-match "@@" default-directory)))
    (if match
        (progn
          (while (and (> match 0)
                      (not (equal (elt default-directory match)
                                  (string-to-char "/"))))
            (setq match (1- match)))


          (let ((new-buffer-name
                 (concat (substring default-directory (1+ match))
                         (buffer-name)))
                (new-dir
                 (substring default-directory 0 (1+ match))))
            (or (string= new-buffer-name (buffer-name))
                ;; Uniquify the name, if necessary.
                ;;
                (let ((n 2)
                      (uniquifier-string ""))
                  (while (get-buffer (concat new-buffer-name uniquifier-string))
                    (setq uniquifier-string (format "<%d>" n))
                    (setq n (1+ n)))
                  (rename-buffer
                   (concat new-buffer-name uniquifier-string))))
            (setq default-directory new-dir)))
        nil)))

(add-hook 'find-file-hooks 'vc-check-cc-name)

(defun vc-find-dir-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (vc-file-clearprops default-directory)
  (if default-directory
      (vc-file-setprop default-directory 'vc-backend nil))

  ;; Let dired decide whether the file should be read-only
  (let (buffer-read-only)
    (vc-mode-line default-directory))

  ;; If in vc-dired-mode, reformat the buffer
  (if vc-dired-mode
      (vc-reformat-dired-buffer)
    ;; Otherwise, check if we should automatically enter vc-dired-mode
    (let ((default-directory (dired-current-directory)))
      (if (and vc-auto-dired-mode
               (or vc-mode
                   (file-directory-p "SCCS")
                   (file-directory-p "RCS")
                   (file-directory-p "CVS")))
        (vc-dired-mode 1))))
  )

(add-hook 'dired-after-readin-hook 'vc-find-dir-hook)

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (save-excursion
        (require 'vc)
        (not (vc-error-occurred (vc-checkout buffer-file-name))))))

(add-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as we'll often
;;; want to call them from random buffers.

(add-to-list 'file-name-handler-alist '("^/view/[^/]+/" . vc-cc-file-handler))
(add-to-list 'file-name-handler-alist '("^/view[/]*$" . vc-cc-view-handler))

; XEmacs - this is preloaded.  let's not be obtuse!
(defconst vc-prefix-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'vc-prefix-map)
    (define-key map "a" 'vc-update-change-log)
    (define-key map "c" 'vc-cancel-version)
    (define-key map "d" 'vc-directory)
    (define-key map "h" 'vc-insert-headers)
    (define-key map "i" 'vc-register)
    (define-key map "l" 'vc-print-log)
    (define-key map "n" 'vc-assign-name)
    (define-key map "r" 'vc-retrieve-snapshot)
    (define-key map "s" 'vc-create-snapshot)
    (define-key map "u" 'vc-revert-buffer)
    (define-key map "v" 'vc-next-action)
    (define-key map "=" 'vc-diff)
    (define-key map "?" 'vc-file-status) ; XEmacs - this doesn't fit elsewhere
    (define-key map "~" 'vc-version-other-window)
    ;; ClearCase only stuff
    (define-key map "e" 'vc-edit-config)
    (define-key map "b" 'vc-cc-browse-versions)
    (define-key map "m" 'vc-mkbrtype)
    (define-key map "t" 'vc-graph-history)
    (define-key map "w" 'vc-cc-what-rule)
    (global-set-key "\C-xv" map)
    map
    ))

;;; Emacs 19 menus.
(if (and (not vc-elucidated) (boundp 'menu-bar-final-items))
    (progn
      (defvar menu-bar-vc-menu
        (make-sparse-keymap "VC"))
      (or (memq
           'vc menu-bar-final-items)
          (setq menu-bar-final-items
                (cons
                 'vc menu-bar-final-items)))
      (define-key menu-bar-vc-menu [vc-report-bug]
        '("Report Bug in Clearcase VC" . cc-vc-submit-bug-report))
      (define-key menu-bar-vc-menu [vc-directory-registered]
        '("List Registered Files" . vc-directory-registered))
      (define-key menu-bar-vc-menu [vc-directory]
        '("List Locked Files Any User" . vc-directory))
      (define-key menu-bar-vc-menu [vc-directory-locked]
        '("List Locked Files" . vc-directory-locked))
      (put 'vc-print-log 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-print-log]
        '("Show Edit History" . vc-print-log))
      (put 'vc-version-other-window 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-version-other-window]
        '("Visit Previous Revision..." . vc-version-other-window))
      (put 'vc-diff-since-revision 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-diff-since-revision]
        '("Show Changes Since Revision..." . vc-diff-since-revision))
      (put 'vc-diff 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-diff]
        '("Show Changes" . vc-diff))
      (put 'vc-rename-this-file 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-rename-this-file]
        '("Rename File" . vc-rename-this-file))
      (put 'vc-revert-buffer 'menu-enable 'vc-mode)
      (define-key menu-bar-vc-menu [vc-revert-buffer]
        '("Revert File" . vc-revert-buffer))
      (define-key menu-bar-vc-menu [vc-update-directory]
        '("Update Current Directory" . vc-update-directory))
      (define-key menu-bar-vc-menu [next-action]
        '("Next Action" . vc-next-action))
      (define-key global-map [menu-bar vc]
        (cons "VC" menu-bar-vc-menu))))

;;; Lucid Emacs menus..
(defconst vc-default-menu
  '(:filter vc-menu-filter
    ["NEXT-OPERATION"                   vc-next-action                  t nil]
    ["Update Current Directory"         vc-update-directory             t]
    "----"
    ["Revert to Last Revision"          vc-revert-buffer                t nil]
    ["Cancel Last Checkin"              vc-cancel-version               t nil]
    ["Rename File"                      vc-rename-this-file             t nil]
    "----"
    ["Diff Against Last Version"        vc-diff                         t]
    ["Diff Between Revisions..."        vc-diff-since-revision          t]
    ["Visit Other Version..."           vc-version-other-window         t]
    ["Show Edit History"                vc-print-log                    t]
    ["Assign Label..."                  vc-assign-name                  t]
    "----"
    ["List Locked Files"                (progn
                                          (setq current-prefix-arg '(16))
                                          (call-interactively 'vc-directory)) t]
    ["List Locked Files Any User"       vc-directory                    t]
    ["List Registered Files"            (progn
                                          (setq current-prefix-arg '(4))
                                          (call-interactively 'vc-directory)) t])
  "Menubar entry for using the revision control system.")

(defconst vc-cvs-menu
  '(:filter vc-menu-filter
    ["Update Current Directory"         vc-cvs-update-directory         t]
    ["Revert File"                      vc-revert-file                  t nil]
    "----"
    ["Show Changes"                     vc-show-changes                 t]
    ["Show Changes Since Revision..."   vc-show-revision-changes        t]
    ["Visit Previous Revision..."       vc-version-other-window         t]
    ["Show File Status"                 vc-cvs-file-status              t]
    ["Show Edit History"                vc-show-history                 t])
  "Menubar entry for using the revision control system with CVS.")

(defconst vc-cc-menu
  '(["Edit Config Spec..." vc-edit-config t]
    ["Browse Versions"     vc-cc-browse-versions t]
    ["Make Branch Type..." vc-mkbrtype t]
    ["View Graph History"  vc-graph-history t]
    ["Show Rule"           vc-cc-what-rule t])
  "Menubar entries to add to the VC menu when using ClearCase.")

;; This function may be called as a menubar hook, or as a menu filter
;; The filter is much more efficient, and doesn't slow down menubar selection
;; for every single menu action, as does the hook method.
(defun vc-menu-filter (&optional menu)
  (if (null menu)
      (progn
        (setq menu (find-menu-item current-menubar '("Tools" "Version Control")))
        ;; Get just the menu portion
        (if menu (setq menu (cdr (car menu))))
        ))
  (if (null menu)
      nil
    (let* ((rest menu)
           (case-fold-search t)
           (filepath (cond ((and vc-dired-mode
                                 (dired-get-filename nil 'no-error)))
                           (buffer-file-name)
                           (t (buffer-name))))
           (file (and filepath (file-name-nondirectory filepath)))
           (vc-file (and filepath (vc-name filepath)))
           owner
           command
           item)
      (while rest
        (setq item (car rest))

        (if (not (vectorp item))
            nil
          (setq command (aref item 1))
          (cond
           ;; Display the correct action for vc-next-action command
           ((eq 'vc-next-action command)
            (aset item 0
                  (cond ((not vc-file)
                         "Register File")
                        ((not (setq owner
                                    ;; Just check properties, it's too
                                    ;; slow (and dangerous) to fetch
                                    ;; properties
                                    (vc-file-getprop filepath 'vc-locking-user)))
                         ;;(vc-locking-user filepath)))
                         "Check out File")
                        ((not (string-equal owner (user-login-name)))
                         "Steal File Lock")
                        (t "Check in File")))
            (aset item 2 (or buffer-file-name
                             (and vc-dired-mode "Marked")))
            )

           ;; Check for commands to disable
           ((memq command
                  '(vc-revert-buffer
                    vc-cancel-version
                    vc-rename-this-file
                    vc-diff
                    vc-diff-since-revision
                    vc-version-other-window
                    vc-visit-previous-revision
                    vc-print-log
                    vc-assign-name))
            (aset item 2 vc-file))

           (t nil))

          ;; Add the file to the menu suffix if not disabled
          (if (and (> (length item) 3) (aref item 2))
              (aset item 3 
                    (if vc-dired-mode "Marked" file)))
          )

        (setq rest (cdr rest)))

      ;; Return menu plus the ClearCase menu if needed
      (if (and vc-file (clearcase-element-p filepath))
          ;; Must use append here - nconc will create looped list
          (append menu '("----") vc-cc-menu)
          menu)
      )))

;; vc-menu-filter was once called vc-sensitize-menu, so just in case another
;; version of vc was loaded:
(defalias 'vc-sensitize-menu 'vc-menu-filter)

(if (and (fboundp 'add-submenu) (not (featurep 'infodock)) vc-elucidated)
    (progn
      (add-submenu '("Tools") (append (list "Version Control") vc-default-menu))
;; Only add the hook if the :filter method is unavailable.  I don't know which
;; version of XEmacs introduced it, but it's been available at least since 19.13
;;      (add-hook 'activate-menubar-hook 'vc-sensitize-menu)))
      ))

;; #### called by files.el.  Define it like this until we're merged.
(defun vc-after-save ())

;;---------------------------------------------------------------------------
;; Utility functions for ClearCase
;;---------------------------------------------------------------------------

(defun clearcase-element-p (path)
  "Determine if PATH refers to a Clearcase element."

  (let (extended-path versioned-path)

    (if (string-match "@@" path)
        (setq extended-path (substring path 0 (match-end 0))
              versioned-path t)
      (setq extended-path (concat path "@@")))

    (and (file-exists-p path)
         (file-directory-p extended-path)

         ;; Non-checked-out elements have the same inode-number
         ;; as the extended name ("foo@@").
         ;; Not so for checked out, and therefore writeable elements.
         ;;
         (or (file-writable-p path)
             versioned-path
             (eq (file-inode path)
                 (file-inode extended-path)))
         )))

(if (not (fboundp 'file-inode))
    (defun file-inode (file)
      (nth 10 (file-attributes file))))

(defun vc-cc-element-name (path)
  (if (string-match "@@" path)
      (substring path 0 (match-beginning 0))
    path))

(defun vc-cc-version-name (path)
  (if (string-match "@@" path)
      (substring path (match-end 0))
    nil))

(defsubst vc-cc-relpath (str)
  (and str
       (stringp str)
       (string-match "^/view/\\([^/]+\\)" str)
       (substring str
                  (match-end 1))))

(defun vc-cc-build-version (file version &optional view-tag)
  "Build a ClearCase version-extended pathname for ELEMENT's version VERSION.
If ELEMENT is actually a version-extended pathname, substitute VERSION for
the version included in ELEMENT.  If VERSION is nil, remove the version-extended
pathname.

If optional VIEW-TAG is specified, make a view-relative pathname, possibly
replacing the existing view prefix."
  (let* ((element (vc-cc-element-name file))
         (glue-fmt (if (and (> (length version) 0)
                            (= (aref version 0) ?/))
                       "%s@@%s"
                     "%s@@/%s"))
         (relpath (vc-cc-relpath element)))
    (if view-tag
        (setq element (concat "/view/" view-tag (or relpath element))))
    (if version
        (format glue-fmt element version)
      element)
    ))

;; These stolen from vc.  pcl-cvs wants to call these in
;; cvs-mark-buffer-changed.  (Basically only changed vc-backend to
;; vc-backend-deduce.)

(defun vc-consult-rcs-headers (file)
  ;; Search for RCS headers in FILE, and set properties
  ;; accordingly.  This function can be disabled by setting
  ;; vc-consult-headers to nil.  
  ;; Returns: nil            if no headers were found 
  ;;                         (or if the feature is disabled,
  ;;                         or if there is currently no buffer 
  ;;                         visiting FILE)
  ;;          'rev           if a workfile revision was found
  ;;          'rev-and-lock  if revision and lock info was found 
  (cond
   ((or (not vc-consult-headers) 
	(not (get-file-buffer file))) nil)
   ((let (status version locking-user)
     (save-excursion
      (set-buffer (get-file-buffer file))
      (goto-char (point-min))
      (cond  
       ;; search for $Id or $Header
       ;; -------------------------
       ((or (and (search-forward "$Id: " nil t)
		 (looking-at "[^ ]+ \\([0-9.]+\\) "))
	    (and (progn (goto-char (point-min))
			(search-forward "$Header: " nil t))
		 (looking-at "[^ ]+ \\([0-9.]+\\) ")))
	(goto-char (match-end 0))
	;; if found, store the revision number ...
	(setq version (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
	;; ... and check for the locking state
	(cond 
	 ((looking-at
	   (concat "[0-9]+[/-][01][0-9][/-][0-3][0-9] "             ; date
	    "[0-2][0-9]:[0-5][0-9]+:[0-6][0-9]+\\([+-][0-9:]+\\)? " ; time
	           "[^ ]+ [^ ]+ "))                       ; author & state
	  (goto-char (match-end 0)) ; [0-6] in regexp handles leap seconds
	  (cond 
	   ;; unlocked revision
	   ((looking-at "\\$")
	    (setq locking-user 'none)
	    (setq status 'rev-and-lock))
	   ;; revision is locked by some user
	   ((looking-at "\\([^ ]+\\) \\$")
	    (setq locking-user
		  (buffer-substring-no-properties (match-beginning 1)
						  (match-end 1)))
	    (setq status 'rev-and-lock))
	   ;; everything else: false
	   (nil)))
	 ;; unexpected information in
	 ;; keyword string --> quit
	 (nil)))
       ;; search for $Revision
       ;; --------------------
       ((re-search-forward (concat "\\$" 
				   "Revision: \\([0-9.]+\\) \\$")
			   nil t)
	;; if found, store the revision number ...
	(setq version (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
	;; and see if there's any lock information
	(goto-char (point-min))
	(if (re-search-forward (concat "\\$" "Locker:") nil t)
	    (cond ((looking-at " \\([^ ]+\\) \\$")
		   (setq locking-user (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 1)))
		   (setq status 'rev-and-lock))
		  ((looking-at " *\\$") 
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock))
		  (t 
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock)))
	  (setq status 'rev)))
       ;; else: nothing found
       ;; -------------------
       (t nil)))
     (if status (vc-file-setprop file 'vc-workfile-version version))
     (and (eq status 'rev-and-lock)
	  (eq (vc-backend-deduce file) 'RCS)
	  (vc-file-setprop file 'vc-locking-user locking-user)
	  ;; If the file has headers, we don't want to query the master file,
	  ;; because that would eliminate all the performance gain the headers
	  ;; brought us.  We therefore use a heuristic for the checkout model 
	  ;; now:  If we trust the file permissions, and the file is not 
          ;; locked, then if the file is read-only the checkout model is 
	  ;; `manual', otherwise `implicit'.
	  (not (vc-mistrust-permissions file))
	  (not (vc-locking-user file))
	  (if (string-match ".r-..-..-." (nth 8 (file-attributes file)))
	      (vc-file-setprop file 'vc-checkout-model 'manual)
	    (vc-file-setprop file 'vc-checkout-model 'implicit)))
     status))))

(defun vc-workfile-version (file)
  ;; Return version level of the current workfile FILE
  ;; This is attempted by first looking at the RCS keywords.
  ;; If there are no keywords in the working file, 
  ;; vc-master-workfile-version is taken.
  ;; Note that this property is cached, that is, it is only 
  ;; looked up if it is nil.
  ;; For SCCS, this property is equivalent to vc-latest-version.
  (cond ((vc-file-getprop file 'vc-workfile-version))
        ((eq (vc-backend-deduce file) 'SCCS) (vc-latest-version file))
        ((eq (vc-backend-deduce file) 'RCS)
         (if (vc-consult-rcs-headers file)
             (vc-file-getprop file 'vc-workfile-version)
           (let ((rev (cond ((vc-master-workfile-version file))
                            ((vc-latest-version file)))))
             (vc-file-setprop file 'vc-workfile-version rev)
             rev)))
        ((eq (vc-backend-deduce file) 'CVS)
         (if (vc-consult-rcs-headers file)   ;; CVS
             (vc-file-getprop file 'vc-workfile-version)
           (catch 'found
             (vc-find-cvs-master (file-name-directory file)
                                 (file-name-nondirectory file)))
           (vc-file-getprop file 'vc-workfile-version)))))

(provide 'vc-cc-hooks)

;;; vc-cc-hooks.el ends here
