;;; vc-clearcase-auto.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (vc-clearcase-start-view vc-clearcase-edcs vc-clearcase-list-view-private-files
;;;;;;  vc-clearcase-label-diff-report vc-clearcase-show-current-activity
;;;;;;  vc-clearcase-set-activity vc-clearcase-update-view vc-clearcase-list-checkouts
;;;;;;  ah-clearcase-file-not-found-handler ah-clearcase-cleartool-program
;;;;;;  vc-clearcase) "vc-clearcase" "vc-clearcase.el" (18076 37290))
;;; Generated autoloads from vc-clearcase.el

(let ((loads (get ... ...))) (if (member (quote "vc-clearcase") loads) nil (put (quote vc-clearcase) (quote custom-loads) (cons ... loads))))

(defvar ah-clearcase-cleartool-program "cleartool" "\
The name of the cleartool executable.")

(custom-autoload (quote ah-clearcase-cleartool-program) "vc-clearcase" t)
(defun vc-clearcase-registered (file)
 (let (wdview
       retcode
       (program ah-clearcase-cleartool-program))
   (setq wdview
         (with-output-to-string
           (with-current-buffer standard-output
             (setq retcode
                   (call-process
                    program nil t nil "pwv" "-short" "-wdview")))))
   ;;(message "Wdview for %s is %S" file wdview)
   (if (or (not (eq retcode 0))
           (eq (compare-strings "** NONE **" 0 10 wdview 0 10) t))
       nil
     (load "vc-clearcase")
     (vc-clearcase-registered file))))

(autoload (quote ah-clearcase-file-not-found-handler) "vc-clearcase" "\
Handle opening of version-extended ClearCase files.
This function should be added to `find-file-not-found-functions'
to handle opening ClearCase files in the format
file.txt@@/main/0.  The function will visit the file first, than
will open the specified version in another window, using
`vc-version-other-window'

\(fn)" nil nil)

(cond ((boundp (quote find-file-not-found-functions)) (add-hook (quote find-file-not-found-functions) (quote ah-clearcase-file-not-found-handler))) ((boundp (quote find-file-not-found-hooks)) (add-hook (quote find-file-not-found-hooks) (quote ah-clearcase-file-not-found-handler))))

(autoload (quote vc-clearcase-list-checkouts) "vc-clearcase" "\
List the checkouts of the current user in DIR.
If PREFIX-ARG is present, an user name can be entered, and all
the views are searched for checkouts of the specified user.  If
the entered user name is empty, checkouts from all the users on
all the views are listed.

\(fn DIR &optional PREFIX-ARG)" t nil)

(autoload (quote vc-clearcase-update-view) "vc-clearcase" "\
Run a cleartool update command in DIR and display the results.
With PREFIX-ARG, run update in preview mode (no actual changes
are made to the views).

\(fn DIR PREFIX-ARG)" t nil)

(autoload (quote vc-clearcase-set-activity) "vc-clearcase" "\
Set the UCM ACTIVITY in the current directory.
In interactive mode, the user is prompted for the available
activities in the stream associated with the UCM view in the
`default-directory', and the selected one is set.

Two special activity names are also accepted: *NONE* which will
cause the current activity to be unset and *NEW-ACTIVITY* which
will create and set a new activity (the user is prompted for the
activity headline).

\(fn &optional ACTIVITY)" t nil)

(autoload (quote vc-clearcase-show-current-activity) "vc-clearcase" "\
Show the current activity in the view.
With prefix arguument (EXTRA-INFO), also shows the number of
files modified under this activity, number of versions and the
number of checked out files.

\(fn &optional EXTRA-INFO)" t nil)

(autoload (quote vc-clearcase-label-diff-report) "vc-clearcase" "\
Report the changed file revisions between labels.
A report is prepared in the *label-diff-report* buffer for the
files in DIR that have different revisions between LABEL-1
and LABEL-2'.

\(fn DIR LABEL-1 LABEL-2)" t nil)

(autoload (quote vc-clearcase-list-view-private-files) "vc-clearcase" "\
List the view private files in DIR.
You can edit the files using 'find-file-at-point'

\(fn DIR)" t nil)

(autoload (quote vc-clearcase-edcs) "vc-clearcase" "\
Fetch the config spec for VIEW-TAG and pop up a buffer with it.
In interactive mode, prompts for a view-tag name with the default
of the current file's view-tag.

\(fn VIEW-TAG)" t nil)

(autoload (quote vc-clearcase-start-view) "vc-clearcase" "\
Start the dynamic view for VIEW-TAG.
In interactive mode, prompts for a view-tag name.

\(fn VIEW-TAG)" t nil)

(define-key vc-prefix-map "e" (quote vc-clearcase-edcs))

(define-key vc-prefix-map "f" (quote vc-clearcase-start-view))

(define-key vc-prefix-map "j" (quote vc-clearcase-gui-vtree-browser))

(define-key vc-prefix-map "k" (quote vc-clearcase-set-activity))

(define-key vc-prefix-map "o" (quote vc-clearcase-list-checkouts))

(define-key vc-prefix-map "p" (quote vc-clearcase-update-view))

(define-key vc-prefix-map "t" (quote vc-clearcase-what-view-tag))

(define-key vc-prefix-map "w" (quote vc-clearcase-what-rule))

(define-key vc-prefix-map "y" (quote vc-clearcase-what-version))

(define-key-after vc-menu-map [separator-clearcase] (quote ("----")) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-version] (quote ("Show file version" . vc-clearcase-what-version)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-rule] (quote ("Show configspec rule" . vc-clearcase-what-rule)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-view-tag] (quote ("Show view tag" . vc-clearcase-what-view-tag)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-gui-vtree-browser] (quote ("Browse version tree (GUI)" . vc-clearcase-gui-vtree-browser)) (quote separator2))

(defvar clearcase-global-menu (let ((m ...)) (define-key m [vc-clearcase-report-bug] (quote ...)) (define-key m [separator-clearcase-1] (quote ...)) (define-key m [vc-clearcase-label-diff-report] (quote ...)) (define-key m [vc-clearcase-list-view-private-files] (quote ...)) (define-key m [vc-clearcase-list-checkouts] (quote ...)) (define-key m [vc-clearcase-update-view] (quote ...)) (define-key m [vc-clearcase-edcs] (quote ...)) (define-key m [vc-clearcase-start-view] (quote ...)) (define-key m [separator-clearcase-2] (quote ...)) (define-key m [vc-clearcase-show-current-activity] (quote ...)) (define-key m [vc-clearcase-set-activity] (quote ...)) (fset (quote clearcase-global-menu) m)))

(define-key-after menu-bar-tools-menu [ah-clearcase] (quote (menu-item "Clearcase" clearcase-global-menu)) (quote vc))

(if (boundp (quote vc-handled-backends)) (unless (memq (quote CLEARCASE) vc-handled-backends) (setq vc-handled-backends (nconc vc-handled-backends ...))) (setq vc-handled-backends (quote (RCS CVS CLEARCASE))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vc-clearcase-auto.el ends here
