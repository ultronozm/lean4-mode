;;; lean4-info.el --- Emacs mode for Lean theorem prover -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016 Gabriel Ebner. All rights reserved.
;;
;; Author: Gabriel Ebner <gebner@gebner.org>
;; Maintainer: Gabriel Ebner <gebner@gebner.org>
;; Created: Oct 29, 2016
;; Keywords: languages
;; Version: 0.1
;; URL: https://github.com/leanprover/lean4-mode
;; SPDX-License-Identifier: Apache-2.0

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at:
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This library provides an advanced LSP feature for `lean4-mode'.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'lean4-syntax)
(require 'lean4-settings)
(require 'lean4-util)
(require 'eglot)
(require 'magit-section)

(defgroup lean4-info nil
  "Lean Info."
  :group 'lean)

;; Lean Info Mode (for "*lean4-info*" buffer)
;; Automode List
;;;###autoload
(define-derived-mode lean4-info-mode prog-mode "Lean-Info"
  "Helper mode for Lean 4 info buffer.
This mode is only used in temporary buffers, for fontification."
  :syntax-table lean4-syntax-table
  :group 'lean
  (set (make-local-variable 'font-lock-defaults) lean4-info-font-lock-defaults))

(defmacro lean4-with-info-output-to-buffer (buffer &rest body)
  "Execute BODY redirecting `print' output to BUFFER."
  `(let ((buf (get-buffer ,buffer)))
     (with-current-buffer buf
       (let ((inhibit-read-only t)
             (standard-output buf))
         (erase-buffer)
         ,@body))))

(defun lean4-ensure-info-buffer (buffer)
  "Create BUFFER if it does not exist.
Also choose settings used for the *Lean Goal* buffer."
  (unless (get-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (buffer-disable-undo)
      (magit-section-mode)
      (set-syntax-table lean4-syntax-table)
      (setq buffer-read-only t))))

(defun lean4-toggle-info-buffer (buffer)
  "Create or delete BUFFER.
The buffer is supposed to be the *Lean Goal* buffer."
  (-if-let (window (get-buffer-window buffer))
      (quit-window nil window)
    (lean4-ensure-info-buffer buffer)
    (display-buffer buffer)))

(defun lean4-info-buffer-active (buffer)
  "Check whether the given info BUFFER should show info for the current buffer."
  (and
   ;; info buffer visible (on any frame)
   (get-buffer-window buffer t)
   ;; current window of current buffer is selected (i.e., in focus)
   (eq (current-buffer) (window-buffer))
   ;; current buffer is visiting a file
   buffer-file-name))

(defconst lean4-info-buffer-name "*Lean Goal*")

(defvar lean4-info--goals nil)
(defvar lean4-info--term-goal nil)

(defun lean4-info--diagnostics ()
  (nreverse
   (cl-loop for diag in (flymake-diagnostics)
            when (cdr (assoc 'eglot-lsp-diag (eglot--diag-data diag)))
            collect it)))

(defun lean4-info--diagnostic-start (diagnostic)
  (eglot--dbind ((Range) start) (cl-getf diagnostic :fullRange)
    (eglot--dbind ((Position) line) start
      line)))

(defun lean4-info--diagnostic-end (diagnostic)
  (eglot--dbind ((Range) end) (cl-getf diagnostic :fullRange)
    (eglot--dbind ((Position) line) end
      line)))

(defun lean4-info--fontify-string (s)
  (with-temp-buffer
    (lean4-info-mode)
    (insert s)
    (font-lock-ensure)
    (buffer-string)))

(defun lean4-mk-message-section (caption errors)
  "Add a section with caption CAPTION and contents ERRORS."
  (when errors
    (magit-insert-section (magit-section)
      (magit-insert-heading caption)
      (magit-insert-section-body
        (dolist (e errors)
          (eglot--dbind ((Diagnostic) message range) e
            (eglot--dbind ((Range) start) range
              (eglot--dbind ((Position) line character) start
                (magit-insert-section (magit-section)
                  (magit-insert-heading (format "%d:%d" (1+ line) character))
                  (magit-insert-section-body
                    (insert message "\n")))))))))))

(defun lean4-info-buffer-redisplay ()
  (let ((inhibit-message t))
    (when (lean4-info-buffer-active lean4-info-buffer-name)
      (-let* ((deactivate-mark)         ; keep transient mark
              (line (save-restriction (widen) (1- (line-number-at-pos nil t))))
              (errors (lean4-info--diagnostics))
              (errors (-sort (-on #'< #'lean4-info--diagnostic-end) errors))
              ((errors-above errors)
               (--split-with (< (lean4-info--diagnostic-end it) line) errors))
              ((errors-here errors-below)
               (--split-with (<= (lean4-info--diagnostic-start it) line) errors)))
        (lean4-with-info-output-to-buffer
         lean4-info-buffer-name
         (when lean4-info--goals
           (magit-insert-section (magit-section)
             (magit-insert-heading "Goals:")
             (magit-insert-section-body
               (if (> (length lean4-info--goals) 0)
                   (seq-doseq (g lean4-info--goals)
                     (magit-insert-section (magit-section)
                       (insert (lean4-info--fontify-string g) "\n\n")))
                 (insert "goals accomplished\n\n")))))
         (when lean4-info--term-goal
           (magit-insert-section (magit-section)
             (magit-insert-heading "Expected type:")
             (magit-insert-section-body
               (insert (lean4-info--fontify-string lean4-info--term-goal) "\n\n"))))
         (lean4-mk-message-section "Messages here:" errors-here)
         (lean4-mk-message-section "Messages below:" errors-below)
         (lean4-mk-message-section "Messages above:" errors-above)
         (when lean4-highlight-inaccessible-names
           (goto-char 0)
           (while (re-search-forward "\\(\\sw+\\)✝\\([¹²³⁴-⁹⁰]*\\)" nil t)
             (replace-match
              (propertize (s-concat (match-string-no-properties 1) (match-string-no-properties 2))
                          'font-lock-face 'font-lock-comment-face)
              'fixedcase 'literal))))))))


;; Debouncing
;; ~~~~~~~~~~~
;; We want to update the Lean4 info buffer as seldom as possible,
;; since magit-section is slow at rendering. We
;; wait a small duration (`debounce-delay-sec') when we get a
;; redisplay request, to see if there is a redisplay request in the
;; future that invalidates the current request (debouncing).
;; Pictorially,
;; (a) One request:
;; --r1
;; --r1.wait
;; ----------r1.render
;; (b) Two requests in quick succession:
;; --r1
;; --r1.wait
;; --------r2(cancel r1.wait)
;; --------r2.wait
;; ---------------r2.render
;; (c) Two requests, not in succession:
;; --r1
;; --r1.wait
;; ---------r1.render
;; ------------------r2
;; ------------------r2.wait
;; -------------------------r2.render
;; This delaying can lead to a pathological case where we continually
;; stagger, while not rendering anything:
;; --r1
;; --r1.wait
;; --------r2(cancel r1.wait)
;; --------r2.wait
;; --------------r3(cancel r2.wait)
;; ---------------r3.wait
;; ---------------------r4(cancel r3.wait)
;; ---------------------...
;; We prevent this pathological case by keeping track of when
;; when we began debouncing in `lean4-info-buffer-debounce-begin-time'.
;; If we have been debouncing for longer than
;; `lean4-info-buffer-debounce-upper-bound-sec', then we
;; immediately write instead of debouncing;
;; `max-debounces' times. Upon trying to stagger the
;; `max-debounces'th request, we immediately render:
;; begin-time:nil----t0----------------nil-------
;;            -------r1                |
;;            -------r1.wait           |
;;            -------|-----r2(cancel r1.wait)
;;            -------|-----r2.wait     |
;;            -------|-----------r3(cancel r2.wait)
;;            -------|-----------r3.wait
;;            -------|-----------------r4(cancel r3.wait)
;;            -------|-----------------|
;;                   >-----------------<
;;                   >longer than `debounce-upper-bound-sec'<
;;            -------------------------r4.render(FORCED)


(defcustom lean4-info-buffer-debounce-delay-sec 0.1
  "Duration of time we wait before writing to *Lean Goal*."
  :group 'lean4-info
  :type 'number)


(defvar lean4-info-buffer-debounce-timer nil
  "Timer that is used to debounce Lean4 info view refresh.")


(defvar lean4-info-buffer-debounce-begin-time nil
  "Return the time we have begun debouncing.

The returned value is nil if we are not currently debouncing.
Otherwise, is a timestamp as given by `current-time'.")

(defcustom lean4-info-buffer-debounce-upper-bound-sec
  0.5
  "Maximum time we are allowed to stagger debouncing.

If we recieve a request such that we have been debouncing for longer than
`lean4-info-buffer-debounce-begin-time', then we immediately run the request."
  :group 'lean4-info
  :type 'number)

;;  Debounce implementation modifed from lsp-lens
;; https://github.com/emacs-lsp/lsp-mode/blob/2f0ea2e396ec9a570f2a2aeb097c304ddc61ebee/lsp-lens.el#L140
(defun lean4-info-buffer-redisplay-debounced ()
  "Debounced version of `lean4-info-buffer-redisplay'.
This version ensures that info buffer is not repeatedly written to.  This is to
prevent lag, because magit is quite slow at building sections."
  ;;  if we have not begun debouncing, setup debouncing begin time.
  (if (not lean4-info-buffer-debounce-begin-time)
      (setq lean4-info-buffer-debounce-begin-time (current-time)))
  ;; if time since we began debouncing is too long...
  (if (>= (time-to-seconds
	   (time-subtract (current-time)
			  lean4-info-buffer-debounce-begin-time))
	  lean4-info-buffer-debounce-upper-bound-sec)
      ;;  then redisplay immediately.
      (progn
	;;  We have stopped debouncing.
	(setq lean4-info-buffer-debounce-begin-time nil)
	(lean4-info-buffer-redisplay))
    ;; else cancel current timer, create new debounced timer.
    (-some-> lean4-info-buffer-debounce-timer cancel-timer)
    (setq lean4-info-buffer-debounce-timer ; set new timer
	  (run-with-timer
	   lean4-info-buffer-debounce-delay-sec
	   nil				; don't repeat timer
	   (lambda ()
	     ;; We have stopped debouncing.
	     (setq lean4-info-buffer-debounce-begin-time nil)
	     (lean4-info-buffer-redisplay))))))

(defun lean4-info-buffer-refresh ()
  "Refresh the *Lean Goal* buffer."
  (let ((inhibit-message t)
        (server (eglot-current-server)))
      (when (and server (lean4-info-buffer-active lean4-info-buffer-name))
        (eglot--signal-textDocument/didChange)
        (jsonrpc-async-request
         server :$/lean/plainGoal (eglot--TextDocumentPositionParams)
         :success-fn (lambda (result)
                       (setq lean4-info--goals (cl-getf result :goals))
                       (lean4-info-buffer-redisplay-debounced)))
        (jsonrpc-async-request
         server :$/lean/plainTermGoal (eglot--TextDocumentPositionParams)
         :success-fn (lambda (result)
                       (setq lean4-info--term-goal (cl-getf result :goal))
                       (lean4-info-buffer-redisplay-debounced))))))

(defun lean4-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (lean4-toggle-info-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

(provide 'lean4-info)
;;; lean4-info.el ends here
