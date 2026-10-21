;;; lispm-shrdlu.el --- SHRDLU, Winograd's blocks world  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-shrdlu
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: games, lisp
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; SHRDLU, which Terry Winograd wrote at MIT in 1968-1970, talked in
;; English about a table of blocks, pyramids and a box, and moved them
;; with a hand: "pick up a big red block", "what does the box
;; contain?".  This runs the Common Lisp port of the 1990s, as Tom
;; Sgouros revived it (www-shrdlu), with fixes kept in LispMachineLab's
;; fork, on Emacs Common Lisp (the emacs-cl package) in a batch Emacs
;; beside this one.  The surviving code is incomplete: SHRDLU no longer
;; knows every word of its famous dialogue.
;;
;; `lispm-shrdlu' opens the dialogue above a drawing of the blocks
;; world, where the hand moves as SHRDLU carries out a command.  The
;; Sentences menu offers the lines of the 1972 dialogue.  The first
;; start fetches SHRDLU's Common Lisp, at `lispm-shrdlu-source-revision',
;; into `lispm-shrdlu-source-directory' with git.  In LispM Desktop it
;; is an application of the App Store.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'comint)
(require 'easymenu)
(require 'svg)
(require 'emacs-cl)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

(defgroup lispm-shrdlu nil
  "SHRDLU, Winograd's blocks world."
  :group 'games
  :prefix "lispm-shrdlu-")

(defcustom lispm-shrdlu-source-url "https://github.com/LispMachineLab/www-shrdlu.git"
  "Where SHRDLU's Common Lisp is fetched from."
  :type 'string)

(defcustom lispm-shrdlu-source-revision "2d61def8bd25d28b75665f7f6f31435900213a30"
  "The commit of `lispm-shrdlu-source-url' SHRDLU runs.
nil runs what `lispm-shrdlu-source-directory' holds, fetching nothing."
  :type '(choice (const :tag "What the directory holds" nil) string))

(defcustom lispm-shrdlu-source-directory (locate-user-emacs-file "lispm-shrdlu/")
  "The checkout of SHRDLU's Common Lisp, its files in src/."
  :type 'directory)

(defcustom lispm-shrdlu-speed 2500
  "How fast the hand moves in the drawing, in SHRDLU's units a second.
The table is 1200 units wide."
  :type 'number)

(defconst lispm-shrdlu-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package.")

(defvar lispm-shrdlu-buffer-name "*SHRDLU*"
  "The name of the dialogue's buffer.")

(defvar lispm-shrdlu-world-buffer-name "*SHRDLU World*"
  "The name of the buffer drawing the blocks world.")

(defconst lispm-shrdlu-introduction
  "SHRDLU talks about the blocks, pyramids and box on its table, and moves
them.  Type a sentence and Return, as: pick up a big red block.  The
Sentences menu holds the dialogue of 1972.\n\n"
  "What a new dialogue starts with.")

(defconst lispm-shrdlu-sentences
  '("pick up a big red block."
    "grasp the pyramid."
    "find a block which is taller than the one you are holding and put it into the box."
    "what does the box contain?"
    "what is the pyramid supported by?"
    "how many blocks are not in the box?"
    "is at least one of them narrower than the one which i told you to pick up?"
    "is it supported?"
    "can the table pick up blocks?"
    "can a pyramid be supported by a block?"
    "can a pyramid support a pyramid?"
    "stack up two pyramids."
    "will you please stack up both of the red blocks and either a green cube or a pyramid?"
    "which cube is sitting on the table?"
    "is there a large block behind a pyramid?"
    "put a small one onto the green cube which supports a pyramid."
    "put the littlest pyramid on top of it."
    "how many things are on top of green cubes?"
    "had you touched any pyramid before you put the green one on the little cube?"
    "when did you pick it up?"
    "why?"
    "how did you do it?"
    "put the blue pyramid on the block in the box."
    "is there anything to the right of the red pyramid?"
    "thank you.")
  "Lines of SHRDLU's dialogue of 1972, as Winograd published it.")

;;;; Fetching SHRDLU's Common Lisp

(defun lispm-shrdlu--source (&optional file)
  "Return the directory of SHRDLU's Common Lisp, or FILE in it."
  (expand-file-name (or file "")
                    (expand-file-name "src/" lispm-shrdlu-source-directory)))

(defun lispm-shrdlu--fetched-p ()
  "Return non-nil when SHRDLU's Common Lisp is there, at the revision."
  (and (file-exists-p (lispm-shrdlu--source "plnr.lisp"))
       (or (null lispm-shrdlu-source-revision)
           (equal lispm-shrdlu-source-revision
                  (with-temp-buffer
                    (let ((default-directory lispm-shrdlu-source-directory))
                      (and (eq 0 (ignore-errors
                                   (call-process "git" nil t nil "rev-parse" "HEAD")))
                           (string-trim (buffer-string)))))))))

(defun lispm-shrdlu--run-git (commands done)
  "Run the git COMMANDS, lists of arguments, one after the other, then
call DONE with nil, or with what git said when one failed."
  (if (null commands)
      (funcall done nil)
    (let ((output (generate-new-buffer " *shrdlu git*")))
      (make-process
       :name "shrdlu-git"
       :buffer output
       :command (cons "git" (car commands))
       :noquery t
       :sentinel (lambda (process _event)
                   (unless (process-live-p process)
                     (let ((said (with-current-buffer output (string-trim (buffer-string)))))
                       (kill-buffer output)
                       (if (eq 0 (process-exit-status process))
                           (lispm-shrdlu--run-git (cdr commands) done)
                         (funcall done (if (string-empty-p said) "git failed" said))))))))))

(defun lispm-shrdlu--fetch (done)
  "Fetch SHRDLU's Common Lisp unless it is there; then call DONE with nil,
or with what went wrong."
  (cond
   ((lispm-shrdlu--fetched-p) (funcall done nil))
   ((null lispm-shrdlu-source-revision)
    (funcall done (format "%s holds no SHRDLU" lispm-shrdlu-source-directory)))
   ((not (executable-find "git"))
    (funcall done "SHRDLU needs git to fetch its Common Lisp"))
   (t
    (let ((directory (expand-file-name lispm-shrdlu-source-directory)))
      (make-directory directory t)
      (lispm-shrdlu--run-git
       `(("init" "-q" ,directory)
         ("-C" ,directory "fetch" "-q" "--depth" "1"
          ,lispm-shrdlu-source-url ,lispm-shrdlu-source-revision)
         ("-C" ,directory "-c" "advice.detachedHead=false" "checkout" "-q" "FETCH_HEAD"))
       done)))))

;;;; The dialogue

(defvar-local lispm-shrdlu--pending ""
  "The start of a report whose line has not come whole.")

(defun lispm-shrdlu--prompt (line)
  "Return the prompt to show for LINE of SHRDLU's, or nil."
  (cond ((string-match-p "\\`READY1 *\n\\'" line) "> ")
        ((string-match-p "\\`READY2 *\n\\'" line) "… ")))

(defun lispm-shrdlu--filter (process string)
  "Show what SHRDLU says in PROCESS's buffer, and take the hand's reports
out of it to the drawing.  READY1 and READY2 become prompts."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((text (concat lispm-shrdlu--pending string))
            (start 0)
            (shown nil))
        (setq lispm-shrdlu--pending "")
        (while (string-match "\n" text start)
          (let ((line (substring text start (match-end 0))))
            (setq start (match-end 0))
            (cond ((string-prefix-p "\036" line)
                   (lispm-shrdlu--report (substring line 1 -1)))
                  ((lispm-shrdlu--prompt line)
                   ;; A prompt starts its own line, after one blank line.
                   (while (and shown (equal (car shown) "\n")) (pop shown))
                   (push "\n" shown)
                   (push (lispm-shrdlu--prompt line) shown))
                  (t (push line shown)))))
        (let ((rest (substring text start)))
          (if (string-prefix-p "\036" rest)
              (setq lispm-shrdlu--pending rest)
            (push rest shown)))
        (let ((output (lispm-shrdlu--tidy (apply #'concat (nreverse shown))
                                          (lispm-shrdlu--blank-lines-before process))))
          (unless (string-empty-p output)
            (comint-output-filter process output)))))))

(defun lispm-shrdlu--blank-lines-before (process)
  "Return how many newlines end the dialogue before PROCESS writes, at most 2."
  (let ((mark (marker-position (process-mark process))) (count 0))
    (while (and (< count 2) (> (- mark count) (point-min))
                (eq (char-before (- mark count)) ?\n))
      (setq count (1+ count)))
    count))

(defun lispm-shrdlu--tidy (output newlines)
  "Return OUTPUT with no more than one blank line in a row, NEWLINES
ending the text before it.  SHRDLU's PRINT and TERPRI leave many."
  (let* ((output (replace-regexp-in-string "\n\\(?:[ \t]*\n\\)\\{2,\\}" "\n\n" output))
         (leading (progn (string-match "\\`\n*" output) (match-end 0))))
    (if (> (+ leading newlines) 2)
        (substring output (min leading (- (+ leading newlines) 2)))
      output)))

(defun lispm-shrdlu--sentinel (process _event)
  "Say in PROCESS's buffer that SHRDLU stopped."
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n\nSHRDLU stopped (%s).  C-c C-n starts again.\n"
                          (process-exit-status process))))))))

(defun lispm-shrdlu--say (buffer text)
  "Insert TEXT in BUFFER, the dialogue, where SHRDLU writes next; point
follows when it was there."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (process (get-buffer-process buffer))
           (mark (if process (process-mark process) (point-max-marker)))
           (follow (>= (point) mark)))
      (save-excursion
        (goto-char mark)
        (insert text)
        (when process (set-marker (process-mark process) (point))))
      (when follow
        (goto-char (if process (process-mark process) (point-max)))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))))))

(defun lispm-shrdlu--start (buffer)
  "Start a dialogue with SHRDLU in BUFFER, fetching it first if need be."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lispm-shrdlu-introduction)))
  (lispm-shrdlu--world-reset)
  (unless (lispm-shrdlu--fetched-p)
    (lispm-shrdlu--say buffer "Fetching SHRDLU's Common Lisp...\n"))
  (lispm-shrdlu--fetch
   (lambda (problem)
     (cond
      ((not (buffer-live-p buffer)))
      (problem
       (lispm-shrdlu--say buffer (format "SHRDLU could not be fetched: %s\n" problem)))
      (t
       (lispm-shrdlu--say buffer "Loading SHRDLU takes a few seconds...\n")
       (let* ((process-environment
               (cons (concat "LISPM_SHRDLU_SOURCE=" (file-name-as-directory (lispm-shrdlu--source)))
                     process-environment))
              (process (emacs-cl-start
                        buffer :name "shrdlu"
                        :load (list (expand-file-name "lisp/shrdlu.lisp" lispm-shrdlu-directory))
                        :package "SHRDLU"
                        :eval '("(lispm-shrdlu-run)"))))
         (set-process-filter process #'lispm-shrdlu--filter)
         (set-process-sentinel process #'lispm-shrdlu--sentinel)))))))

(defun lispm-shrdlu-running-p ()
  "Return non-nil while SHRDLU runs in the dialogue's buffer."
  (let ((buffer (get-buffer lispm-shrdlu-buffer-name)))
    (and buffer (process-live-p (get-buffer-process buffer)))))

(defun lispm-shrdlu--buffers ()
  "Return the dialogue's buffer, made ready, after making the world's."
  (with-current-buffer (get-buffer-create lispm-shrdlu-world-buffer-name)
    (unless (derived-mode-p 'lispm-shrdlu-world-mode)
      (lispm-shrdlu-world-mode)))
  (let ((buffer (get-buffer-create lispm-shrdlu-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-shrdlu-mode)
        (lispm-shrdlu-mode)))
    buffer))

;;;###autoload
(defun lispm-shrdlu ()
  "Talk with SHRDLU, going on with the dialogue under way."
  (interactive)
  (let ((buffer (lispm-shrdlu--buffers)))
    (unless (get-buffer-process buffer)
      (lispm-shrdlu--start buffer))
    (pop-to-buffer-same-window buffer)
    (lispm-shrdlu--show-world (selected-window))))

(defun lispm-shrdlu-new ()
  "Start a new dialogue, the blocks back where they began."
  (interactive)
  (let ((buffer (lispm-shrdlu--buffers)))
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (lispm-shrdlu--start buffer)
    (pop-to-buffer-same-window buffer)
    (lispm-shrdlu--show-world (selected-window))))

(defun lispm-shrdlu-say (sentence)
  "Say SENTENCE to SHRDLU, as if typed."
  (interactive (list (completing-read "Say: " lispm-shrdlu-sentences)))
  (with-current-buffer (lispm-shrdlu--buffers)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (process-live-p process)
        (user-error "SHRDLU is not running; C-c C-n starts it"))
      (goto-char (process-mark process))
      (delete-region (point) (point-max))
      (insert sentence)
      (comint-send-input))))

(defun lispm-shrdlu--sentences-menu (_menu)
  "Return the items of the Sentences menu."
  (mapcar (lambda (sentence)
            (vector sentence (lambda () (interactive) (lispm-shrdlu-say sentence))
                    :active '(lispm-shrdlu-running-p)))
          lispm-shrdlu-sentences))

(defvar-keymap lispm-shrdlu-mode-map
  :parent comint-mode-map
  "C-c C-n" #'lispm-shrdlu-new
  "C-c C-s" #'lispm-shrdlu-say
  ;; Comint's menus are a shell's.
  "<menu-bar> <signals>" #'undefined
  "<menu-bar> <inout>" #'undefined
  "<menu-bar> <completion>" #'undefined)

(easy-menu-define lispm-shrdlu-sentences-menu lispm-shrdlu-mode-map
  "The sentences of SHRDLU's dialogue of 1972."
  '("Sentences" :filter lispm-shrdlu--sentences-menu))

;; Defined after Sentences, to come before it in the menu bar.
(easy-menu-define lispm-shrdlu-menu lispm-shrdlu-mode-map
  "Menu of the dialogue with SHRDLU."
  '("SHRDLU"
    ["New Dialogue" lispm-shrdlu-new]
    ["Say a Sentence of 1972..." lispm-shrdlu-say (lispm-shrdlu-running-p)]))

(defun lispm-shrdlu--keep-world (window)
  "Show the blocks world below WINDOW, now showing the dialogue.
Later: this runs as redisplay starts, when windows had better not change."
  (run-at-time 0 nil
               (lambda ()
                 (when (and (window-live-p window)
                            (eq (window-buffer window) (get-buffer lispm-shrdlu-buffer-name)))
                   (lispm-shrdlu--show-world window)))))

(define-derived-mode lispm-shrdlu-mode comint-mode "SHRDLU"
  "Major mode of the dialogue with SHRDLU, opened by `lispm-shrdlu'.
Type a sentence and Return.
\\{lispm-shrdlu-mode-map}"
  (setq-local comint-prompt-regexp "^[>…] ")
  (setq-local comint-prompt-read-only t)
  (add-hook 'window-buffer-change-functions #'lispm-shrdlu--keep-world nil t))

;;;; The blocks world

(defvar lispm-shrdlu--kinds nil
  "What each thing is: ((NAME KIND COLOR) ...), strings, from SHRDLU.")

(defvar lispm-shrdlu--objects nil
  "Where each thing is, as drawn: ((NAME (X Y Z) (DX DY DZ)) ...).")

(defvar lispm-shrdlu--hand '(40 0 0)
  "Where the hand is, as drawn.")

(defvar lispm-shrdlu--held nil
  "What the hand holds, as drawn.")

(defvar lispm-shrdlu--queue nil
  "The reports not drawn yet, oldest first.")

(defvar lispm-shrdlu--motion nil
  "The hand's move being drawn: (START DURATION POINTS), POINTS from where
it was to where it goes.")

(defvar lispm-shrdlu--blinking nil
  "(NAME . UNTIL): the thing SHRDLU points at, until when.")

(defvar lispm-shrdlu--timer nil
  "The timer drawing moves.")

(defun lispm-shrdlu--world-reset ()
  "Forget the world: it is reported again as SHRDLU starts."
  (when lispm-shrdlu--timer
    (cancel-timer lispm-shrdlu--timer))
  (setq lispm-shrdlu--objects nil lispm-shrdlu--hand '(40 0 0) lispm-shrdlu--held nil
        lispm-shrdlu--queue nil lispm-shrdlu--motion nil lispm-shrdlu--blinking nil
        lispm-shrdlu--timer nil)
  (lispm-shrdlu--render))

(defun lispm-shrdlu--report (line)
  "Take LINE, a report of SHRDLU's hand, to the drawing."
  (let ((report (ignore-errors (car (read-from-string line)))))
    (cond
     ((plist-get report :kinds)
      (setq lispm-shrdlu--kinds
            (mapcar (lambda (kind) (cons (lispm-shrdlu--name (car kind)) (cdr kind)))
                    (plist-get report :kinds))))
     ((plist-get report :event)
      (setq lispm-shrdlu--queue (nconc lispm-shrdlu--queue (list report)))
      (lispm-shrdlu--animate)))))

(defun lispm-shrdlu--name (name)
  "Return NAME, a keyword or a string, as a string without its colon."
  (string-remove-prefix ":" (format "%s" name)))

(defun lispm-shrdlu--place (name)
  "Return the entry of `lispm-shrdlu--objects' of NAME."
  (seq-find (lambda (object) (equal (lispm-shrdlu--name (car object)) (lispm-shrdlu--name name)))
            lispm-shrdlu--objects))

(defun lispm-shrdlu--distance (a b)
  "Return the distance from point A to point B."
  (sqrt (cl-loop for p in a for q in b sum (expt (- q p) 2))))

(defun lispm-shrdlu--path-length (points)
  "Return the length of the path through POINTS."
  (cl-loop for (a b) on points while b sum (lispm-shrdlu--distance a b)))

(defun lispm-shrdlu--along (points distance)
  "Return the point DISTANCE along the path through POINTS."
  (let ((point (car points)))
    (while (and (cdr points) distance)
      (let* ((a (car points)) (b (cadr points))
             (length (lispm-shrdlu--distance a b)))
        (if (and (> length 0) (<= distance length))
            (setq point (cl-mapcar (lambda (p q) (+ p (* (- q p) (/ distance length)))) a b)
                  distance nil)
          (setq point b distance (- distance length))))
      (setq points (cdr points)))
    point))

(defun lispm-shrdlu--carry ()
  "Put what the hand holds under it: the hand holds a thing at the
centre of its top."
  (when-let* ((held (and lispm-shrdlu--held (lispm-shrdlu--place lispm-shrdlu--held))))
    (pcase-let ((`(,x ,y ,z) lispm-shrdlu--hand)
                (`(,dx ,dy ,dz) (nth 2 held)))
      (setf (nth 1 held) (list (- x (/ dx 2.0)) (- y (/ dy 2.0)) (- z dz))))))

(defun lispm-shrdlu--begin (report now)
  "Begin drawing REPORT at NOW; return non-nil when it takes time."
  (pcase (plist-get report :event)
    ((or :start :sync)
     (setq lispm-shrdlu--hand (plist-get report :hand)
           lispm-shrdlu--objects (copy-tree (plist-get report :objects)))
     (lispm-shrdlu--carry)
     nil)
    (:move
     (let* ((from lispm-shrdlu--hand)
            (to (plist-get report :to))
            ;; SHRDLU's hand goes straight; the drawn one goes up first.
            (points (if (and (equal (car from) (car to)) (equal (cadr from) (cadr to)))
                        (list from to)
                      (list from (list (car from) (cadr from) 1000)
                            (list (car to) (cadr to) 1000) to))))
       (setq lispm-shrdlu--motion
             (list now (max 0.15 (/ (lispm-shrdlu--path-length points) lispm-shrdlu-speed))
                   points))))
    (:grasp
     (setq lispm-shrdlu--held (plist-get report :object))
     (setq lispm-shrdlu--motion (list now 0.15 (list lispm-shrdlu--hand))))
    (:release
     (setq lispm-shrdlu--held nil)
     (setq lispm-shrdlu--motion (list now 0.15 (list lispm-shrdlu--hand))))
    (:blink
     (setq lispm-shrdlu--blinking (cons (plist-get report :object) (+ now 0.9)))
     (setq lispm-shrdlu--motion (list now 0.9 (list lispm-shrdlu--hand))))))

(defun lispm-shrdlu--step ()
  "Draw the world as it is now, going on with the reports."
  (let ((now (float-time)))
    (while (and (null lispm-shrdlu--motion) lispm-shrdlu--queue)
      (lispm-shrdlu--begin (pop lispm-shrdlu--queue) now))
    (when lispm-shrdlu--motion
      (pcase-let ((`(,start ,duration ,points) lispm-shrdlu--motion))
        (let ((fraction (min 1.0 (/ (- now start) duration))))
          (setq lispm-shrdlu--hand
                (lispm-shrdlu--along points (* fraction (lispm-shrdlu--path-length points))))
          (lispm-shrdlu--carry)
          (when (>= fraction 1.0)
            (setq lispm-shrdlu--hand (car (last points))
                  lispm-shrdlu--motion nil)
            (lispm-shrdlu--carry)))))
    (when (and lispm-shrdlu--blinking (> now (cdr lispm-shrdlu--blinking)))
      (setq lispm-shrdlu--blinking nil))
    (lispm-shrdlu--render)
    (unless (or lispm-shrdlu--motion lispm-shrdlu--queue lispm-shrdlu--blinking)
      (when lispm-shrdlu--timer
        (cancel-timer lispm-shrdlu--timer)
        (setq lispm-shrdlu--timer nil)))))

(defun lispm-shrdlu--animate ()
  "Draw the reports waiting, moving the hand as time passes."
  (unless lispm-shrdlu--timer
    (setq lispm-shrdlu--timer (run-at-time 0 0.04 #'lispm-shrdlu--step))))

;;;; Drawing

(defconst lispm-shrdlu--colors
  '(("RED" . "#DD0806") ("GREEN" . "#1FB714") ("BLUE" . "#0000D4")
    ("WHITE" . "#F2F2F2") ("BLACK" . "#222222"))
  "The colours of SHRDLU's things, Mac OS's own.")

(defconst lispm-shrdlu--view '(-40 -40 1820 1560)
  "The part of the projected world drawn: X Y WIDTH HEIGHT.")

(defun lispm-shrdlu--project (point)
  "Return the drawing's (X . Y) of POINT, (X Y Z) of SHRDLU's world.
The table is seen from its front, above and to the right."
  (pcase-let ((`(,x ,y ,z) point))
    (cons (+ x (* 0.45 y)) (- 1480 (+ z (* 0.3 y))))))

(defun lispm-shrdlu--mix (color other amount)
  "Return COLOR, a #RRGGBB string, AMOUNT of the way to OTHER."
  (let ((a (color-name-to-rgb color)) (b (color-name-to-rgb other)))
    (apply #'color-rgb-to-hex (append (cl-mapcar (lambda (p q) (+ p (* (- q p) amount))) a b)
                                      '(2)))))

(defun lispm-shrdlu--polygon (svg points fill &optional stroke opacity)
  "Draw on SVG the polygon through POINTS of the world, filled with FILL,
OPACITY opaque, fully by default."
  (svg-polygon svg (mapcar #'lispm-shrdlu--project points)
               :fill fill :fill-opacity (or opacity 1) :stroke (or stroke "#000000")
               :stroke-width 4 :stroke-linejoin "round"))

(defun lispm-shrdlu--kind (name)
  "Return (KIND COLOR) of the thing NAME."
  (let ((name (lispm-shrdlu--name name)))
    (or (cdr (assoc name lispm-shrdlu--kinds))
        (if (string-prefix-p "bw" name t) '("WALL" "WHITE") '("BLOCK" "WHITE")))))

(defun lispm-shrdlu--draw-thing (svg object)
  "Draw OBJECT, (NAME (X Y Z) (DX DY DZ)), on SVG."
  (pcase-let* ((`(,name (,x0 ,y0 ,z0) (,dx ,dy ,dz)) object)
               (`(,kind ,color) (lispm-shrdlu--kind name))
               (base (or (cdr (assoc color lispm-shrdlu--colors)) "#F2F2F2"))
               (top (lispm-shrdlu--mix base "#FFFFFF" 0.35))
               (side (lispm-shrdlu--mix base "#000000" 0.3))
               (x1 (+ x0 dx)) (y1 (+ y0 dy)) (z1 (+ z0 dz))
               ;; The box's walls let what it holds be seen.
               (opacity (if (member kind '("WALL" "BOX")) 0.3 1))
               (stroke (if (and lispm-shrdlu--blinking
                                (equal (lispm-shrdlu--name name)
                                       (lispm-shrdlu--name (car lispm-shrdlu--blinking)))
                                (cl-evenp (floor (* 5 (float-time)))))
                           "#FCF305" "#000000")))
    (if (equal kind "PYRAMID")
        (let ((apex (list (+ x0 (/ dx 2.0)) (+ y0 (/ dy 2.0)) z1)))
          (lispm-shrdlu--polygon svg (list (list x1 y0 z0) (list x1 y1 z0) apex) side stroke)
          (lispm-shrdlu--polygon svg (list (list x0 y0 z0) (list x1 y0 z0) apex) base stroke))
      (lispm-shrdlu--polygon svg (list (list x1 y0 z0) (list x1 y1 z0) (list x1 y1 z1) (list x1 y0 z1))
                             side stroke opacity)
      (lispm-shrdlu--polygon svg (list (list x0 y0 z1) (list x1 y0 z1) (list x1 y1 z1) (list x0 y1 z1))
                             top stroke opacity)
      (lispm-shrdlu--polygon svg (list (list x0 y0 z0) (list x1 y0 z0) (list x1 y0 z1) (list x0 y0 z1))
                             base stroke opacity))))

(defun lispm-shrdlu--draw-hand (svg)
  "Draw the hand, hanging from above, on SVG."
  (pcase-let* ((`(,x ,y ,z) lispm-shrdlu--hand)
               (`(,ax . ,ay) (lispm-shrdlu--project (list x y 1150)))
               (`(,hx . ,hy) (lispm-shrdlu--project (list x y z)))
               (open (if lispm-shrdlu--held 0 25)))
    (svg-line svg ax ay hx (- hy 30) :stroke "#555555" :stroke-width 14)
    (svg-line svg (- hx 70 open) (- hy 30) (+ hx 70 open) (- hy 30)
              :stroke "#555555" :stroke-width 14 :stroke-linecap "round")
    (dolist (finger (list (- hx 70 open) (+ hx 70 open)))
      (svg-line svg finger (- hy 30) finger (+ hy 20)
                :stroke "#555555" :stroke-width 12 :stroke-linecap "round"))))

(defun lispm-shrdlu--depth (object)
  "Return the key drawing OBJECT after those behind or below it."
  (pcase-let ((`(,_ (,x ,y ,z) (,_ ,dy ,_)) object))
    (list (- (+ y dy)) x z)))

(defun lispm-shrdlu--key< (a b)
  "Return non-nil when the key A comes before B."
  (cl-loop for p in a for q in b
           if (< p q) return t
           if (> p q) return nil))

(defun lispm-shrdlu-world-svg (width height)
  "Return the blocks world drawn as an SVG of WIDTH and HEIGHT pixels."
  (let ((svg (svg-create width height
                         :viewBox (mapconcat #'number-to-string lispm-shrdlu--view " "))))
    (pcase-let ((`(,vx ,vy ,vw ,vh) lispm-shrdlu--view))
      (svg-rectangle svg vx vy vw vh :fill "#FFFFFF"))
    (lispm-shrdlu--polygon svg '((0 0 0) (1200 0 0) (1200 1200 0) (0 1200 0)) "#E6E6E6" "#999999")
    (dolist (object (sort (copy-sequence lispm-shrdlu--objects)
                          (lambda (a b) (lispm-shrdlu--key< (lispm-shrdlu--depth a)
                                                            (lispm-shrdlu--depth b)))))
      (lispm-shrdlu--draw-thing svg object))
    (lispm-shrdlu--draw-hand svg)
    svg))

(defun lispm-shrdlu--render ()
  "Draw the world in its buffer, to the size of its window."
  (when-let* ((buffer (get-buffer lispm-shrdlu-world-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (window (get-buffer-window buffer t)))
        (erase-buffer)
        (if (not (and window (display-images-p) lispm-shrdlu--objects))
            (insert (if lispm-shrdlu--objects "" "The blocks world appears once SHRDLU has loaded.\n"))
          (pcase-let* ((`(,_ ,_ ,vw ,vh) lispm-shrdlu--view)
                       (room-width (max 50 (- (window-body-width window t) 4)))
                       (room-height (max 50 (- (window-body-height window t) 4)))
                       (width (min room-width (floor (* room-height (/ (float vw) vh)))))
                       (height (floor (* width (/ (float vh) vw)))))
            (insert (propertize " " 'display `(space :align-to (,(/ (- room-width width) 2)))))
            (insert-image (svg-image (lispm-shrdlu-world-svg width height)) " ")))))))

(defun lispm-shrdlu--resized (_frame)
  "Draw the world again for its window's new size."
  (when (get-buffer-window lispm-shrdlu-world-buffer-name t)
    (lispm-shrdlu--render)))

(define-derived-mode lispm-shrdlu-world-mode special-mode "SHRDLU World"
  "Major mode of the drawing of SHRDLU's blocks world."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  ;; Under the dialogue, in the same window of LispM Desktop.
  (setq-local tab-line-format nil)
  (add-hook 'window-size-change-functions #'lispm-shrdlu--resized nil t))

(defun lispm-shrdlu--show-world (window)
  "Show the blocks world below WINDOW, in a window of its own: the
dialogue keeps the title bar, the world the window's bottom."
  (let ((world (get-buffer lispm-shrdlu-world-buffer-name)))
    (when (and world (not (get-buffer-window world (window-frame window))))
      ;; Where the window is too small to split, the dialogue goes alone.
      (when-let* ((below (ignore-errors
                           (split-window window (- (floor (* 0.55 (window-total-height window))))
                                         'below))))
        (set-window-buffer below world)
        (set-window-dedicated-p below t)
        (set-window-parameter below 'no-other-window t)
        (lispm-shrdlu--render)))))

;;;; In LispM Desktop

(defun lispm-shrdlu--status ()
  "Return what the window's bottom says."
  (if (lispm-shrdlu-running-p) "Talking" "Stopped"))

(with-eval-after-load 'lispm-desktop-app-ui
  (lispm-desktop-app-ui-define
   'lispm-shrdlu-mode
   :title "SHRDLU"
   :prose t
   :buttons '(("New Dialogue" lispm-shrdlu-new)
              ("Say…" lispm-shrdlu-say lispm-shrdlu-running-p))
   :label (lambda () "Type a sentence, then Return")
   :bottom nil)
  (lispm-desktop-app-ui-define
   'lispm-shrdlu-world-mode
   :title "SHRDLU"
   :buttons nil
   :cursor nil
   :status #'lispm-shrdlu--status))

(defun lispm-shrdlu-unload-function ()
  "Stop SHRDLU and its drawing; then let unloading go on."
  (when lispm-shrdlu--timer
    (cancel-timer lispm-shrdlu--timer))
  (when-let* ((buffer (get-buffer lispm-shrdlu-buffer-name))
              (process (get-buffer-process buffer)))
    (delete-process process))
  nil)

(provide 'lispm-shrdlu)
;;; lispm-shrdlu.el ends here
