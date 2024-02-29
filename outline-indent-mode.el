;;; outline-indent-mode.el --- Dynamic indentation based on outline -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Meow King <mr.meowking@anche.no>
;; Keywords: outlines, text
;; URL:   ;FIXME
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.
;;
;; The process is synchronous, toggled at every buffer modification.
;; Though, the initialization (indentation of text already in the
;; buffer), which can take a few seconds in large buffers, happens on
;; idle time.

;; The implementation of the dynamic virtual indentation feature largely
;; refers to the `org-indent-mode'.  Credits belongs to it.

;;; Code:

(require 'outline)

(defgroup outline-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Outline Indent"
  :group 'outline-ts)

(defcustom outline-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :type 'natnum
  :group 'outline-indent)

(defvar-local outline-indent-head-indentation-char ?\s
  "The characters that appear at the beginning of an indented headline.")

(defcustom outline-indent-boundary-char ?\s
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so.
This can be useful on a terminal window - under a windowing system,
it may be prettier to customize the `outline-indent' face."
  :type 'character
  :group 'outline-indent)

(defface outline-indent '((t (:inherit org-indent)))
  ;; inherit org-indent face since some theme customize org faces
  "Face for outline indentation.
The default is to make it look like white space.  But you may find it
useful to make it ever so slightly different."
  :group 'outline-indent)

(defvar outline-indent--initial-marker nil
  "Position of initialization before interrupt.
This is used locally in each buffer being initialized.")

(defvar outline-indent--heading-line-prefixes nil
  "Vector containing line prefix strings for headlines.")

(defvar outline-indent--text-line-prefixes nil
  "Vector containing line prefixes strings for regular text.")

(defconst outline-indent--deepest-level 50
  "Maximum theoretical headline depth.")

(defvar outline-indent-modified-headline-flag nil
  "Non-nil means the last deletion operated on a headline.
It is modified by `outline-indent-notify-modified-headline'.")

(defvar outline-indent-agent-timer nil
  "Timer running the initialize agent.")

(defvar outline-indent-agent-resume-timer nil
  "Timer to reschedule agent after switching to other idle processes.")

(defvar outline-indent-agent-active-delay '(0 2 0)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize is current.")

(defvar outline-indent-agent-passive-delay '(0 0 400000)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize isn't current.")

(defvar outline-indent-agent-resume-delay '(0 0 100000)
  "Minimal time for other idle processes before switching back to agent.")

(defvar outline-indent-agentized-buffers nil
  "List of buffers watched by the initialize agent.")

(defun outline-indent-add-props (string plist &rest props)
  "Add text properties to entire STRING, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)

(defun outline-indent--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."
  (setq outline-indent--heading-line-prefixes
        (make-vector outline-indent--deepest-level nil))
  (setq outline-indent--text-line-prefixes
        (make-vector outline-indent--deepest-level nil))
  (when (> outline-indent-indentation-per-level 0)
    (dotimes (n outline-indent--deepest-level)
      (let ((indentation (if (<= n 1) 0
                           (* (1- outline-indent-indentation-per-level)
                              (1- n)))))
        ;; Headlines line prefixes.
        (let ((heading-prefix (make-string indentation outline-indent-head-indentation-char)))
          (aset outline-indent--heading-line-prefixes
                n
                (outline-indent-add-props heading-prefix nil 'face 'outline-indent)))
        ;; Text line prefixes.
        (aset outline-indent--text-line-prefixes
              n
              (outline-indent-add-props
               (concat (make-string (+ n indentation) ?\s)
                       (and (> n 0)
                            (char-to-string outline-indent-boundary-char)))
               nil 'face 'outline-indent))))))

(defun outline-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
                          '(line-prefix nil wrap-prefix nil) string)
  string)

(defmacro outline-indent-with-wide-buffer (&rest body)
  "Execute body while temporarily widening the buffer.
BODY: code to be executed."
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defun outline-indent-get-outline-regexp-bol ()
  "Return `outline-regexp' with line beginning character."
  (if (string-prefix-p "^" outline-regexp)
      outline-regexp
    (concat "^" outline-regexp)))

(defun outline-indent-initialize-agent ()
  "Start or resume current buffer initialization.
Only buffers in `outline-indent-agentized-buffers' trigger an action.
When no more buffer is being watched, the agent suppress itself."
  (when outline-indent-agent-resume-timer
    (cancel-timer outline-indent-agent-resume-timer))
  (setq outline-indent-agentized-buffers
        (cl-remove-if-not #'buffer-live-p outline-indent-agentized-buffers))
  (cond
   ;; Job done:  kill agent.
   ((not outline-indent-agentized-buffers) (cancel-timer outline-indent-agent-timer))
   ;; Current buffer is agentized: start/resume initialization
   ;; somewhat aggressively.
   ((memq (current-buffer) outline-indent-agentized-buffers)
    (outline-indent-initialize-buffer (current-buffer)
                                      outline-indent-agent-active-delay))
   ;; Else, start/resume initialization of the last agentized buffer,
   ;; softly.
   (t (outline-indent-initialize-buffer (car outline-indent-agentized-buffers)
                                        outline-indent-agent-passive-delay))))

(defun outline-indent-initialize-buffer (buffer delay)
  "Set virtual indentation for the buffer BUFFER, asynchronously.
Give hand to other idle processes if it takes longer than DELAY,
a time value."
  (with-current-buffer buffer
    (when (bound-and-true-p outline-indent-mode)
      (outline-indent-with-wide-buffer
       (let ((interruptp
              ;; Always nil unless interrupted.
              (catch 'interrupt
                (and outline-indent--initial-marker
                     (marker-position outline-indent--initial-marker)
                     (equal (marker-buffer outline-indent--initial-marker)
                            buffer)
                     (outline-indent-add-properties outline-indent--initial-marker
                                                    (point-max)
                                                    delay)
                     nil))))
         (move-marker outline-indent--initial-marker interruptp)
         ;; Job is complete: un-agentize buffer.
         (unless interruptp
           (setq outline-indent-agentized-buffers
                 (delq buffer outline-indent-agentized-buffers))))))))

(defun outline-indent-set-line-properties (level indentation &optional heading)
  "Set prefix properties on current line an move to next one.

LEVEL is the current level of heading.  INDENTATION is the
expected indentation when wrapping line.

When optional argument HEADING is non-nil, assume line is at
a heading."
  (let* ((line (aref (pcase heading
                       (`nil outline-indent--text-line-prefixes)
                       (_ outline-indent--heading-line-prefixes))
                     level))
         (wrap
          (outline-indent-add-props
           (concat line
                   (if heading (concat (make-string
                                        level
                                        outline-indent-head-indentation-char)
                                       " ")
                     (make-string indentation ?\s)))
           nil 'face 'outline-indent)))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

(defun outline-indent-add-properties (beg end &optional delay)
  "Add indentation properties between BEG and END.

When DELAY is non-nil, it must be a time value.  In that case,
the process is asynchronous and can be interrupted, either by
user request, or after DELAY.  This is done by throwing the
`interrupt' tag along with the buffer position where the process
stopped."
  (save-match-data
    (outline-indent-with-wide-buffer
     (goto-char beg)
     (beginning-of-line)
     ;; Initialize prefix at BEG, according to current entry's level.
     (let* ((case-fold-search t)
            ;; NOTE markdown mode's `markdown-outline-level' reports wrong level
            (level (or (funcall outline-level) 0))
            (time-limit (and delay (time-add nil delay))))
       ;; For each line, set `line-prefix' and `wrap-prefix'
       ;; properties depending on the type of line (headline, inline
       ;; task, item or other).
       (with-silent-modifications
         (while (and (<= (point) end) (not (eobp)))
           (cond
            ;; When in asynchronous mode, check if interrupt is
            ;; required.
            ((and delay (input-pending-p)) (throw 'interrupt (point)))
            ;; In asynchronous mode, take a break of
            ;; `outline-indent-agent-resume-delay' every DELAY to avoid
            ;; blocking any other idle timer or process output.
            ((and delay (time-less-p time-limit nil))
             (setq outline-indent-agent-resume-timer
                   (run-with-idle-timer
                    (time-add (current-idle-time) outline-indent-agent-resume-delay)
                    nil #'outline-indent-initialize-agent))
             (throw 'interrupt (point)))
            ;; Headline
            ((looking-at outline-regexp)
             (setq level (or (funcall outline-level) 1))
             (outline-indent-set-line-properties level 0 t))
            ;; Regular line.
            (t
             (outline-indent-set-line-properties
              level
              (current-indentation))))))))))

(defun outline-indent-refresh-maybe (beg end _)
  "Refresh indentation properties in an adequate portion of buffer.
BEG and END are the positions of the beginning and end of the
range of inserted text.  DUMMY is an unused argument.

This function is meant to be called by `after-change-functions'."
  (when (bound-and-true-p outline-indent-mode)
    (save-match-data
      ;; If a headline was modified or inserted, set properties until
      ;; next headline.
      (outline-indent-with-wide-buffer
       (if (or outline-indent-modified-headline-flag
               (save-excursion
                 (goto-char beg)
                 (beginning-of-line)
                 (re-search-forward
                  (outline-indent-get-outline-regexp-bol)
                  (save-excursion
                    (goto-char end)
                    ;; Extend to headline if END is within its
                    ;; headline stars.
                    (line-end-position))
                  t)))
           (let ((end (save-excursion
                        (goto-char end)
                        (outline-next-heading)
                        (point))))
             (setq outline-indent-modified-headline-flag nil)
             (outline-indent-add-properties beg end))
         ;; Otherwise, only set properties on modified area.
         (outline-indent-add-properties beg end))))))

(defun outline-indent-at-heading-p ()
  "Return t if point is on a (possibly invisible) heading line.
If INVISIBLE-NOT-OK is non-nil, an invisible heading line is not ok."
  (save-excursion
    (beginning-of-line)
    (looking-at outline-regexp)))

(defun outline-indent-notify-modified-headline (beg end)
  "Set `outline-indent-modified-headline-flag' depending on context.

BEG and END are the positions of the beginning and end of the
range of deleted text.

This function is meant to be called by `before-change-functions'.
Flag will be non-nil if command is going to modify or delete an
headline."
  (when (bound-and-true-p outline-indent-mode)
    (setq outline-indent-modified-headline-flag
          (outline-indent-with-wide-buffer
           (goto-char beg)
           (save-match-data
             (or (and (outline-indent-at-heading-p) (< beg (match-end 0)))
                 (re-search-forward
                  (outline-indent-get-outline-regexp-bol) end t)))))))

(defsubst outline-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(define-minor-mode outline-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time."
  :lighter " Ind"
  (cond
   (outline-indent-mode
    ;; mode was turned on.
    (setq-local indent-tabs-mode nil)
    (setq-local outline-indent--initial-marker (copy-marker 1))
    (outline-indent--compute-prefixes)
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'outline-indent-remove-properties-from-string)
    (add-hook 'after-change-functions 'outline-indent-refresh-maybe nil 'local)
    (add-hook 'before-change-functions
              'outline-indent-notify-modified-headline nil 'local)
    (outline-indent-remove-properties (point-min) (point-max))
    ;; Submit current buffer to initialize agent.  If it's the first
    ;; buffer submitted, also start the agent.  Current buffer is
    ;; pushed in both cases to avoid a race condition.
    (if outline-indent-agentized-buffers
        (push (current-buffer) outline-indent-agentized-buffers)
      (push (current-buffer) outline-indent-agentized-buffers)
      (setq outline-indent-agent-timer
            (run-with-idle-timer 0.2 t #'outline-indent-initialize-agent))))
   (t
    ;; Mode was turned off (or we refused to turn it on)
    (setq outline-indent-agentized-buffers
          (delq (current-buffer) outline-indent-agentized-buffers))
    (when (markerp outline-indent--initial-marker)
      (set-marker outline-indent--initial-marker nil))
    (remove-function (local 'filter-buffer-substring-function)
                     #'outline-indent-remove-properties-from-string)
    (remove-hook 'after-change-functions 'outline-indent-refresh-maybe 'local)
    (remove-hook 'before-change-functions
                 'outline-indent-notify-modified-headline 'local)
    (outline-indent-with-wide-buffer
     (outline-indent-remove-properties (point-min) (point-max))))))

(provide 'outline-indent-mode)

;;; outline-indent-mode.el ends here
