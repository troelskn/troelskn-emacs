;;; textpad.el --- Makes Emacs behave more like Textpad

;; Copyright (C) 2007 Troels Knak-Nielsen

;; Author: Troels Knak-Nielsen <troelskn@gmail.com>
;; Maintainer: Troels Knak-Nielsen <troelskn@gmail.com>
;; Created: 2 July 2007

;;; Commentary:
;;
;; This library makes cursor movement behave like in the popular text
;; editor Textpad. It extends on pc-selection-mode.

;; To install, add the following to your .emacs file
;; (require 'textpad)
;; (textpad-mode)

;; This is my first attempt at writing a library for emacs, so any comments
;; on how to do things smarter is welcome.

;; Some of the code was lifted from the next-screen-line library

(defconst textpad-version "0.1")

;;; next-screen-line.el --- A next-line that moves by screen lines.
;; Copyright (c) 1992 by Govind N. Kamat <kamat@ece.UC.EDU>
;; May be copied, modified and distributed under the
;; GNU Emacs General Public License.
;;
;; Author: Govind N. Kamat <kamat@ece.UC.EDU>
;; Version: 2.1
;; Keywords: next-line wrap line movement scrolling

(defvar next-line-move-newlines nil
  "*Make next-line and previous-line move by newline-separated lines instead of
screen lines.")

(setq running-xemacs nil)

(defsubst ensure-mark ()
  ;; make sure mark is active
  ;; test if it is active, if it isn't, set it and activate it
  (if running-xemacs
    (progn (unless zmacs-region-active-p
             (zmacs-activate-region) (set-mark-command nil))
           (setq zmacs-region-stays t))
    (progn (unless mark-active (set-mark-command nil))
           (setq deactivate-mark nil))))

(defsubst ensure-nomark ()
  ;; make sure mark is inactive
  (if running-xemacs
    (zmacs-deactivate-region)
    (setq mark-active nil)))

(defun textpad-forward-word (&optional arg)
  (interactive "p")
  (dotimes (n arg)
    (goto-char (1+ (point)))
    (re-search-forward "\\b" nil t)))

(defun textpad-backward-word (&optional arg)
  (interactive "p")
  (dotimes (n arg)
    (goto-char (1- (point)))
    (re-search-backward "\\b" nil t)))

(defun next-screen-line-next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.).

If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(next-screen-line-next-line-internal arg)
	(if (= opoint (point))
      (error "End of buffer")))
    (next-screen-line-next-line-internal arg))
  (and (= scroll-step 1)
       (> arg 0)
       (or (pos-visible-in-window-p) (recenter -1)))
  nil)

(defun next-screen-line-previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with negative argument instead..  It is usually easier
to use and more reliable (no dependence on goal column, etc.).

If `next-line-move-newlines' is nil, moves by screen lines instead of
newline-separated ones."
  (interactive "p")
  (next-screen-line-next-line-internal (- arg))
  (and (= scroll-step 1)
       (> arg 0)
       (or (pos-visible-in-window-p) (recenter 0)))
  nil)

(defun next-screen-line-next-line-internal (arg)
  (if (not (memq last-command '(next-line previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		t
	      (current-column))
	    next-screen-line-next-line-internal-flag nil))
;  (let* ((w (1- (window-width)))
  (let* ((w (window-width)) ;Modified by troelskn@gmail.com
	 (g (or goal-column temporary-goal-column))
	 (pos (if (eq g t) w (% g w)))
	 c)
    (and (= pos 0)
	 (> g 0)
	 (or (and (> (current-column) 0)
		  (= (or (char-after (point)) 10) 10))
	     (>= (or goal-column 0) w))
	 (setq next-screen-line-next-line-internal-flag t))
    (if (or next-line-move-newlines
	    ;; maintain compatibility when not interactive
	    (not (memq this-command '(next-line previous-line))))
	(if (not (integerp selective-display))
	    (forward-line arg)
	  (while (> arg 0)
	    (end-of-line)
	    (vertical-motion 1)
	    (setq arg (1- arg)))
	  (while (< arg 0)
	    (beginning-of-line)
	    (vertical-motion -1)
	    (setq arg (1+ arg))))
      (vertical-motion arg)
      (and (eobp) (vertical-motion 0)))
    (cond
     ((eq g t) (end-of-line))
     ((or next-line-move-newlines
	  truncate-lines
	  (> (window-hscroll) 0)
	  (and (< w (1- (screen-width))) truncate-partial-width-windows))
      (move-to-column g))
     (t (setq g (point) c (current-column))
	(setq c (* w (/ c w)))
	(move-to-column (+ (if next-screen-line-next-line-internal-flag w pos) c))
	(and (/= g (save-excursion (vertical-motion 0) (point)))
	     (forward-char -1))))))

;; goal-column-magic and back-to-indentation

(defgroup textpad nil
  "Make emacs behave like Textpad"
  :group 'convenience)

(defun textpad-ensure-mark()
  ;; make sure mark is active
  ;; test if it is active, if it isn't, set it and activate it
  (and (not mark-active) (set-mark-command nil)))

(defun textpad-mode-set-goal-column ()
  ""
  (setq goal-column
    (current-column)))

(defun textpad-mode-backward-char-nomark (&optional arg)
  ""
  (interactive "p")
  (let ((is-mark-active mark-active))
    (setq mark-active nil)
    (if is-mark-active
        (goto-char (min (point) (mark)))
      (backward-char arg)))
  (textpad-mode-set-goal-column))

(defun textpad-mode-forward-char-nomark (&optional arg)
  ""
  (interactive "p")
  (let ((is-mark-active mark-active))
    (setq mark-active nil)
    (if is-mark-active
        (goto-char (max (point) (mark)))
      (forward-char arg)))
  (textpad-mode-set-goal-column))

(defun textpad-mode-back-to-indentation-or-beginning (&optional arg)
  ""
  (interactive "p")
  (setq mark-active nil)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-back-to-indentation-or-beginning-mark (&optional arg)
  ""
  (interactive "p")
  (textpad-ensure-mark)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-end-of-line-mark (&optional arg)
  ""
  (interactive "p")
  (textpad-ensure-mark)
  (end-of-line arg)
  (textpad-mode-set-goal-column))

(defun textpad-mode-end-of-line (&optional arg)
  ""
  (interactive "p")
  (setq mark-active nil)
  (end-of-line arg)
  (textpad-mode-set-goal-column))

(defun textpad-mode-backward-word-nomark (&optional arg)
  ""
  (interactive "p")
  (ensure-nomark)
  (unless (bobp) (textpad-backward-word arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-forward-word-nomark (&optional arg)
  ""
  (interactive "p")
  (ensure-nomark)
  (unless (eobp) (textpad-forward-word arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-backward-char-mark (&optional arg)
  ""
  (interactive "p")
  (backward-char-mark arg)
  (textpad-mode-set-goal-column))

(defun textpad-mode-forward-char-mark (&optional arg)
  ""
  (interactive "p")
  (forward-char-mark arg)
  (textpad-mode-set-goal-column))

(defun textpad-mode-backward-word-mark (&optional arg)
  ""
  (interactive "p")
  (ensure-mark)
  (unless (bobp) (textpad-backward-word arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-forward-word-mark (&optional arg)
  ""
  (interactive "p")
  (ensure-mark)
  (unless (eobp) (textpad-forward-word arg))
  (textpad-mode-set-goal-column))

(defun textpad-mode-next-line-nomark (&optional arg)
  ""
  (interactive "p")
  (setq mark-active nil)
  (setq this-command 'next-line)
  (next-screen-line-next-line arg))

(defun textpad-mode-next-line-mark (&optional arg)
  ""
  (interactive "p")
  (textpad-ensure-mark)
  (setq this-command 'next-line)
  (next-screen-line-next-line arg))

(defun textpad-mode-previous-line-nomark (&optional arg)
  ""
  (interactive "p")
  (setq mark-active nil)
  (setq this-command 'previous-line)
  (next-screen-line-previous-line arg))

(defun textpad-mode-previous-line-mark (&optional arg)
  ""
  (interactive "p")
  (textpad-ensure-mark)
  (setq this-command 'previous-line)
  (next-screen-line-previous-line arg))

(defun textpad-mode ()
  "Installs textpad-mode"
  (interactive)
  (when (not pc-selection-mode)
    (pc-selection-mode))
  (global-set-key (kbd "<home>") 'textpad-mode-back-to-indentation-or-beginning)
  (global-set-key (kbd "<end>") 'textpad-mode-end-of-line)
  (global-set-key (kbd "<S-home>") 'textpad-mode-back-to-indentation-or-beginning-mark)
  (global-set-key (kbd "<S-end>") 'textpad-mode-end-of-line-mark)
  (global-set-key (kbd "<left>") 'textpad-mode-backward-char-nomark)
  (global-set-key (kbd "<right>") 'textpad-mode-forward-char-nomark)
  (global-set-key (kbd "<C-left>") 'textpad-mode-backward-word-nomark)
  (global-set-key (kbd "<C-right>") 'textpad-mode-forward-word-nomark)
  (global-set-key (kbd "<S-left>") 'textpad-mode-backward-char-mark)
  (global-set-key (kbd "<S-right>") 'textpad-mode-forward-char-mark)
  (global-set-key (kbd "<C-S-left>") 'textpad-mode-backward-word-mark)
  (global-set-key (kbd "<C-S-right>") 'textpad-mode-forward-word-mark)
  (global-set-key (kbd "<up>") 'textpad-mode-previous-line-nomark)
  (global-set-key (kbd "<down>") 'textpad-mode-next-line-nomark)
  (global-set-key (kbd "<S-up>") 'textpad-mode-previous-line-mark)
  (global-set-key (kbd "<S-down>") 'textpad-mode-next-line-mark))

(provide 'textpad)