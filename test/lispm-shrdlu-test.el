;;; lispm-shrdlu-test.el --- Tests of lispm-shrdlu.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; The drawing is tested from reports made up here; the dialogue runs
;; SHRDLU's Common Lisp from SHRDLU_SOURCE, a checkout of www-shrdlu
;; (../www-shrdlu by default), with Common Lisp built in a temporary
;; directory.  Fetching is tested from a git repository made here.

;;; Code:

(require 'ert)
(require 'lispm-shrdlu)

(defvar lispm-shrdlu-test-source
  (file-name-as-directory (expand-file-name (or (getenv "SHRDLU_SOURCE") "../www-shrdlu")))
  "A checkout of www-shrdlu.")

(defvar lispm-shrdlu-test-build (make-temp-file "lispm-shrdlu-test-build-" t)
  "The build directory of Common Lisp for the tests.")

(defun lispm-shrdlu-test-settle (&optional seconds)
  "Draw the reports waiting until none is left, within SECONDS."
  (let ((end (+ (float-time) (or seconds 30))))
    (while (and (or lispm-shrdlu--queue lispm-shrdlu--motion lispm-shrdlu--blinking)
                (< (float-time) end))
      (lispm-shrdlu--step)
      (sleep-for 0.02))
    (null (or lispm-shrdlu--queue lispm-shrdlu--motion))))

(defmacro lispm-shrdlu-test-world (&rest body)
  "Run BODY with a world of its own."
  `(let ((lispm-shrdlu--kinds nil) (lispm-shrdlu--objects nil) (lispm-shrdlu--hand '(40 0 0))
         (lispm-shrdlu--held nil) (lispm-shrdlu--queue nil) (lispm-shrdlu--motion nil)
         (lispm-shrdlu--blinking nil) (lispm-shrdlu--timer nil) (lispm-shrdlu-speed 100000))
     (unwind-protect (progn ,@body)
       (when lispm-shrdlu--timer (cancel-timer lispm-shrdlu--timer)))))

(defconst lispm-shrdlu-test-start
  "(:event :start :hand (40 0 0) :objects ((:b6 (0 300 0) (200 300 300)) (:b7 (0 240 300) (200 200 200)) (:b4 (640 640 1) (200 200 200))))")

(ert-deftest lispm-shrdlu-prompts ()
  "SHRDLU's READY1 and READY2 become prompts."
  (should (equal (lispm-shrdlu--prompt "READY1 \n") "> "))
  (should (equal (lispm-shrdlu--prompt "READY2 \n") "… "))
  (should-not (lispm-shrdlu--prompt "Ok. \n")))

(ert-deftest lispm-shrdlu-hand-carries ()
  "The hand goes where SHRDLU says, carrying what it grasped at the centre
of its top, and leaves it where it let go."
  (lispm-shrdlu-test-world
   (lispm-shrdlu--report "(:kinds ((:b6 \"BLOCK\" \"RED\") (:b4 \"PYRAMID\" \"BLUE\")))")
   (should (equal (lispm-shrdlu--kind :b6) '("BLOCK" "RED")))
   (should (equal (lispm-shrdlu--kind :bw1) '("WALL" "WHITE")))
   (lispm-shrdlu--report lispm-shrdlu-test-start)
   (dolist (report '("(:event :move :to (100 340 500))" "(:event :grasp :object :b7)"
                     "(:event :move :to (900 285 200))" "(:event :release)"
                     "(:event :move :to (100 450 300))"))
     (lispm-shrdlu--report report))
   (should (lispm-shrdlu-test-settle))
   (should (equal lispm-shrdlu--hand '(100 450 300)))
   (should-not lispm-shrdlu--held)
   (should (equal (nth 1 (lispm-shrdlu--place :b7)) '(800.0 185.0 0)))
   (should (equal (nth 1 (lispm-shrdlu--place :b6)) '(0 300 0)))))

(ert-deftest lispm-shrdlu-hand-goes-up-first ()
  "The drawn hand rises before it goes across, then comes down."
  (lispm-shrdlu-test-world
   (lispm-shrdlu--report lispm-shrdlu-test-start)
   (lispm-shrdlu-test-settle)
   (lispm-shrdlu--begin '(:event :move :to (900 285 200)) 0)
   (should (equal (nth 2 lispm-shrdlu--motion)
                  '((40 0 0) (40 0 1000) (900 285 1000) (900 285 200))))
   (should (equal (lispm-shrdlu--along '((0 0 0) (0 0 1000) (300 0 1000)) 1100)
                  '(100.0 0.0 1000.0)))))

(ert-deftest lispm-shrdlu-sync ()
  "A sync puts everything where SHRDLU has it."
  (lispm-shrdlu-test-world
   (lispm-shrdlu--report lispm-shrdlu-test-start)
   (lispm-shrdlu--report "(:event :sync :hand (1 2 3) :objects ((:b6 (500 500 0) (200 300 300))))")
   (should (lispm-shrdlu-test-settle))
   (should (equal lispm-shrdlu--hand '(1 2 3)))
   (should (equal lispm-shrdlu--objects '((:b6 (500 500 0) (200 300 300)))))))

(ert-deftest lispm-shrdlu-drawing ()
  "The world is drawn: the table, a polygon a face, and the hand."
  (lispm-shrdlu-test-world
   (lispm-shrdlu--report "(:kinds ((:b6 \"BLOCK\" \"RED\") (:b4 \"PYRAMID\" \"BLUE\")))")
   (lispm-shrdlu--report lispm-shrdlu-test-start)
   (lispm-shrdlu-test-settle)
   (let* ((svg (lispm-shrdlu-world-svg 600 512))
          (text (with-temp-buffer (svg-print svg) (buffer-string))))
     ;; The table, three faces of each block, two of the pyramid.
     (should (= (how-many-in "<polygon" text) (+ 1 3 3 2)))
     (should (string-search "#DD0806" text))
     (should (string-search "#0000D4" text))
     (should (= (how-many-in "<line" text) 4)))))

(defun how-many-in (regexp string)
  "Return how many matches for REGEXP STRING holds."
  (with-temp-buffer (insert string) (goto-char (point-min)) (how-many regexp)))

(ert-deftest lispm-shrdlu-fetch ()
  "SHRDLU's Common Lisp is fetched at the revision, and found there after."
  (let* ((origin (make-temp-file "lispm-shrdlu-origin-" t))
         (checkout (make-temp-file "lispm-shrdlu-checkout-" t))
         (default-directory origin))
    (unwind-protect
        (progn
          (make-directory "src")
          (with-temp-file "src/plnr.lisp" (insert "(print :plnr)\n"))
          (dolist (command '(("init" "-q") ("add" ".")
                             ("-c" "user.name=t" "-c" "user.email=t@t" "-c" "commit.gpgsign=false"
                              "commit" "-q" "-m" "SHRDLU")))
            (should (eq 0 (apply #'call-process "git" nil nil nil command))))
          (let* ((revision (string-trim (shell-command-to-string "git rev-parse HEAD")))
                 (lispm-shrdlu-source-url origin)
                 (lispm-shrdlu-source-revision revision)
                 (lispm-shrdlu-source-directory (file-name-as-directory checkout))
                 (result 'waiting))
            (should-not (lispm-shrdlu--fetched-p))
            (lispm-shrdlu--fetch (lambda (problem) (setq result problem)))
            (let ((end (+ (float-time) 60)))
              (while (and (eq result 'waiting) (< (float-time) end))
                (accept-process-output nil 0.1)))
            (should (null result))
            (should (lispm-shrdlu--fetched-p))
            (should (file-exists-p (lispm-shrdlu--source "plnr.lisp")))))
      (delete-directory origin t)
      (delete-directory checkout t))))

(defun lispm-shrdlu-test-wait-for (regexp &optional seconds)
  "Wait until the dialogue holds a match for REGEXP after the last one
waited for; return non-nil when it came within SECONDS."
  (let ((end (+ (float-time) (or seconds 120))) found)
    (while (and (not (setq found (save-excursion (goto-char lispm-shrdlu-test--from)
                                                 (re-search-forward regexp nil t))))
                (< (float-time) end))
      (accept-process-output (get-buffer-process (current-buffer)) 0.1))
    (when found (set-marker lispm-shrdlu-test--from found))
    found))

(defvar-local lispm-shrdlu-test--from nil
  "Where the next wait looks from: comint moves point with the output.")

(ert-deftest lispm-shrdlu-dialogue ()
  "SHRDLU picks up a big red block, clearing it first, and says what the
box contains; the drawing follows the hand."
  (skip-unless (file-exists-p (expand-file-name "src/plnr.lisp" lispm-shrdlu-test-source)))
  (let ((emacs-cl-build-directory lispm-shrdlu-test-build)
        (lispm-shrdlu-source-directory lispm-shrdlu-test-source)
        (lispm-shrdlu-source-revision nil)
        (lispm-shrdlu-speed 100000))
    (save-window-excursion
      (lispm-shrdlu-new)
      (with-current-buffer lispm-shrdlu-buffer-name
        (setq lispm-shrdlu-test--from (copy-marker (point-min)))
        (unwind-protect
            (progn
              (should (lispm-shrdlu-test-wait-for "^> "))
              (should (lispm-shrdlu--place :b6))
              (lispm-shrdlu-say "pick up a big red block.")
              (should (lispm-shrdlu-test-wait-for "^Ok\\."))
              (should (lispm-shrdlu-test-wait-for "^> "))
              (should (lispm-shrdlu-test-settle))
              (should (equal (lispm-shrdlu--name lispm-shrdlu--held) "b6"))
              (should (equal lispm-shrdlu--hand '(100 450 1000)))
              (lispm-shrdlu-say "what does the box contain?")
              (should (lispm-shrdlu-test-wait-for "^The blue pyramid\\."))
              (should-not (string-search "\036" (buffer-string)))
              (should-not (string-search "READY" (buffer-string))))
          (let ((process (get-buffer-process (current-buffer))))
            (when process (set-process-sentinel process #'ignore) (delete-process process))))))))

;;; lispm-shrdlu-test.el ends here
