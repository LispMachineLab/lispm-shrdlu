;;;; shrdlu.lisp --- Loads SHRDLU's Common Lisp, and its hand's reports.
;;;;
;;;; Copyright (C) 2026  Ryan.K
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Loads the files of SHRDLU in the directory LISPM_SHRDLU_SOURCE names,
;;;; in the SHRDLU package, as its loader.lisp does.  Its remote.lisp names
;;;; CLISP's socket package, for the web display it is not asked to drive
;;;; here: a package of that name lets it be read.  Its warnings about
;;;; variables set before being defined are kept out of the dialogue.
;;;;
;;;; Then SHRDLU's hand reports what it does on lines starting with the
;;;; character 30, which lispm-shrdlu.el reads to draw the blocks world:
;;;; (:kinds ((NAME KIND COLOR) ...)) once; (:event :move :to (X Y Z)),
;;;; (:event :grasp :object NAME), (:event :release) and (:event :blink
;;;; :object NAME) as the hand moves; and (:event :start or :sync :hand
;;;; (X Y Z) :objects ((NAME (X Y Z) (DX DY DZ)) ...)), the positions of
;;;; ATABLE, at the start and before each sentence is read.  SHRDLU plans
;;;; a command whole, ATABLE and all, before its hand carries it out, so
;;;; ATABLE holds the end of the moves while they are reported.
;;;;
;;;; lispm-shrdlu-run talks with the user, going on after an error.
;;;; Emacs Common Lisp warns whenever SHRDLU sets a variable it did not
;;;; declare: *ERROR-OUTPUT* goes nowhere.

(defpackage :socket (:use :common-lisp) (:export #:socket-connect))

(defpackage :shrdlu (:use :common-lisp))

(in-package :shrdlu)

(defconstant chrct 76)

(let ((directory (el:|getenv| "LISPM_SHRDLU_SOURCE"))
      (*error-output* (make-broadcast-stream)))
  (dolist (name '("remote" "fixes" "progmr" "cgram" "syscom" "smutil" "smspec"
                  "smass" "dictio" "morpho" "show" "blockl" "plnr" "setup"
                  "thtrac" "blockp" "data" "newans" "mover"))
    (load (concatenate 'string directory name ".lisp"))))

(setq remote-cmds-p nil
      remote-actions-p nil)

(setq *error-output* (make-broadcast-stream))

(defun lispm-report (plist)
  "Write PLIST on a line of its own for lispm-shrdlu.el."
  (let ((*print-pretty* nil)
        (*print-case* :downcase))
    (format t "~&~C~S~%" (code-char 30) plist)))

(defun lispm-report-world (event)
  (lispm-report (list :event event
                      :hand handat
                      :objects (mapcar (lambda (entry)
                                         (list (first entry) (second entry) (third entry)))
                                       atable))))

(defun lispm-report-kinds ()
  (lispm-report
   (list :kinds (mapcar (lambda (entry)
                          (list (first entry)
                                (string-left-trim "#" (symbol-name (third entry)))
                                (symbol-name (sixth entry))))
                        display-as))))

;;; In place of mover.lisp's, which wrote ~MOVING HAND TO ...~.

(defun moveto (x y z)
  (lispm-report (list :event :move :to (list x y z))))

(defun grasp (x)
  (lispm-report (list :event :grasp :object x)))

(defun ungrasp ()
  (lispm-report (list :event :release)))

(defun blink (x)
  (lispm-report (list :event :blink :object x)))

(let ((etaoin (symbol-function 'etaoin)))
  (setf (symbol-function 'etaoin)
        (lambda ()
          (lispm-report-world :sync)
          (funcall etaoin))))

(defun lispm-describe (condition)
  "What CONDITION says."
  (el:|emacs-cl-inferior--describe| condition))

(defun lispm-shrdlu-run ()
  "Talk with the user until the input ends; after an error, go on with
the next sentence."
  (usermode)
  (lispm-report-kinds)
  (lispm-report-world :start)
  (let ((first t))
    (loop
      (handler-case
          (progn
            (if first
                (progn (setq first nil) (initialstuff 'umr-1.0 nil))
                (shrdlu))
            (return))
        (end-of-file () (return))
        (error (condition)
          (format t "~&~%I got confused there (~A).~%" (lispm-describe condition)))))))
