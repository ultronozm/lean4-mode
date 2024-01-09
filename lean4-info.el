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
  (declare (indent 2)
           (debug (form &rest form)))
  (let ((buf-var (make-symbol "buf")))
    `(let ((,buf-var (get-buffer ,buffer)))
       (with-current-buffer ,buf-var
         (let ((inhibit-read-only t)
               (standard-output ,buf-var))
           (erase-buffer)
           ,@body)))))

(defun lean4-ensure-info-buffer (buffer)
  "Create BUFFER if it does not exist.
Also choose settings used for the *Lean Goal* buffer."
  (unless (get-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (magit-section-mode)
      (buffer-disable-undo)
      (add-hook 'window-configuration-change-hook
                #'lean4--idle-invalidate nil t)
      (add-hook 'eldoc-documentation-functions #'lean4-info-eldoc-function
                nil t)
      (set-input-method "Lean")
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
(defvar lean4-info--server nil)
(defvar lean4-info--textDocument nil)
(defvar lean4-info--sessionId nil)
(defvar lean4-info--position nil)

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

(defvar lean4-info-plain t)

(defun lean4--uri ()
  (plist-get
   (plist-get (eglot--TextDocumentPositionParams) :textDocument)
   :uri))

(defun lean4--session-id-sync ()
  (let*
      ((server (eglot-current-server))
       (uri (lean4--uri))
       (response (jsonrpc-request server :$/lean/rpc/connect `(:uri ,uri))))
    (plist-get response :sessionId)))

(defun lean4-info-parse-goal (goal)
  (let* ((userName (plist-get goal :userName))
         (type (plist-get goal :type))
         (hyps (plist-get goal :hyps))
         (goalPrefix (plist-get goal :goalPrefix))
         (ctx (plist-get goal :ctx))
         (p (plist-get ctx :p)))
    (concat
     (when userName (concat "case " userName "\n"))
     (mapconcat (lambda (hyp)
                  (lean4-info-parse-hyp hyp p))
                hyps "\n")
     "\n"
     goalPrefix
     (lean4-info-parse-type type p))))

(defun lean4-info-parse-hyp (hyp p)
  (let* ((type (plist-get hyp :type))
         (names (plist-get hyp :names)))
    (concat (mapconcat #'identity names
                       (propertize " " 'lean4-p p))
            (propertize " : " 'lean4-p p)
            (lean4-info-parse-type type p))))

(defun lean4-info-parse-type (type _p)
  (let* ((tag (plist-get type :tag)))
    (lean4-info-parse-tag tag)))

(defun lean4-info-parse-tag (tag)
  (let* ((tag0 (aref tag 0))
         (info (plist-get tag0 :info))
         (p (plist-get info :p))
         (tag1 (aref tag 1)))
    (lean4-info-parse-expr tag1 p)))

(defun lean4-info-parse-expr (expr &optional p)
  (cond
   ((equal (car expr) :text)
    (if p
        (propertize (cadr expr) 'lean4-p p)
      (cadr expr)))
   ((equal (car expr) :append)
    (mapconcat (lambda (item)
                 (cond
                  ((equal (car item) :text)
                   (if p
                       (propertize (cadr item) 'lean4-p p)
                     (cadr item)))
                  ((equal (car item) :tag)
                   (lean4-info-parse-tag (cadr item)))))
               (cadr expr)))))

(defun lean4-info-buffer-refresh ()
  "Refresh the *Lean Goal* buffer."
  (let* ((server (eglot-current-server))
         (buf (current-buffer))
         (goals :none)
         (term-goal :none)
         (handle-response
          (lambda ()
            (when (and (not (eq goals :none))
                       (not (eq term-goal :none))
                       (buffer-live-p buf))
              ;; print goals and term-goal to *DebugInfo* buffer:
              (with-current-buffer (get-buffer-create "*DebugInfo*")
                (erase-buffer)
                (insert (format "goals: %s\n" goals))
                (insert (format "term-goal: %s\n" term-goal)))
              (with-current-buffer buf
                (setq lean4-info--goals goals)
                (setq lean4-info--term-goal term-goal)
                (lean4-info-buffer-redisplay))))))
    (when (and server (lean4-info-buffer-active lean4-info-buffer-name))
      (eglot--signal-textDocument/didChange)
      (let* ((textdoc-pos (eglot--TextDocumentPositionParams))
             (textDocument (plist-get textdoc-pos :textDocument))
             (position (plist-get textdoc-pos :position)))
        (if lean4-info-plain
            (progn
              (jsonrpc-async-request
               server :$/lean/plainGoal (eglot--TextDocumentPositionParams)
               :success-fn (lambda (result)
                             (setq goals (cl-getf result :goals))
                             (funcall handle-response)))
              (jsonrpc-async-request
               server :$/lean/plainTermGoal (eglot--TextDocumentPositionParams)
               :success-fn (lambda (result)
                             (setq term-goal (cl-getf result :goal))
                             (funcall handle-response))))
          ;; We save these for the sake of subsequent requests made
          ;; from the info buffer, where they are not directly
          ;; available.
          (setq lean4-info--server server)
          (setq lean4-info--textDocument textDocument)
          ;; the following should really be called once per "session",
          ;; but I'm not sure what that actually means.
          (setq lean4-info--sessionId (lean4--session-id-sync))
          (setq lean4-info--position position)
          (jsonrpc-async-request
           server :$/lean/rpc/call
           `(:method "Lean.Widget.getInteractiveGoals"
                     :sessionId ,lean4-info--sessionId
                     :textDocument ,textDocument
                     :position ,position
                     :params (:textDocument ,textDocument
                                            :position ,position
                                            ))
           :success-fn
           (lambda (result)
             (setq goals (when result
                           (vconcat (mapcar #'lean4-info-parse-goal
                                            (cl-getf result :goals)))))
             (funcall handle-response)))
          (jsonrpc-async-request
           server :$/lean/rpc/call
           `(:method "Lean.Widget.getInteractiveTermGoal"
                     :sessionId ,lean4-info--sessionId
                     :textDocument ,textDocument
                     :position ,position
                     :params (:textDocument ,textDocument
                                            :position ,position
                                            ))
           :success-fn
           (lambda (result)
             (setq term-goal (when result (lean4-info-parse-goal result)))
             (funcall handle-response))))))))

(defun lean4-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (lean4-toggle-info-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

;; don't need this function anymore, should delete it eventually
(defun lean4-info-docs (&optional pos)
  "Get hover docs at point in info buffer."
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((p (get-text-property pos 'lean4-p))
         (whatever)
         (handle-response
          (lambda ()
            (message whatever))))
    (when (and lean4-info--server p)
      (jsonrpc-async-request
       lean4-info--server :$/lean/rpc/call
       `(:method "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
                 :sessionId ,lean4-info--sessionId
                 :textDocument ,lean4-info--textDocument
                 :position ,lean4-info--position
                 :params (:p ,p))
       :success-fn
       (lambda (result)
         (with-current-buffer (get-buffer-create "*DebugInfoResults*")
           (erase-buffer)
           (insert (format "result: %s" result)))
         (let ((doc (plist-get result :doc))
               (type (lean4-info-parse-type (plist-get result :type)
                                            nil)))
           (setq whatever (format "type: %s\ndoc: %s" type doc)))
         (funcall handle-response))))))

(defun lean4-info--widget-region (&optional pos)
  (unless pos (setq pos (point)))
  (when-let ((p (get-text-property pos 'lean4-p)))
    (let ((start (point-min))
          (end (point-max)))
      (while (and start
                  (< start (point-max))
                  (not (eq (get-text-property start 'lean4-p) p)))
        (setq start (next-single-property-change start 'lean4-p nil (point-max))))
      (while (and end
                  (> end (point-min))
                  (not (eq (get-text-property end 'lean4-p) p)))
        (setq end (previous-single-property-change end 'lean4-p nil (point-min))))
      (setq end (next-single-property-change end 'lean4-p nil (point-max)))
      (cons start end))))

(defun lean4-info-eldoc-function (cb)
  "Eldoc function for info buffer."
  (unless lean4-info-plain
    (let* ((pos (point))
           (p (get-text-property pos 'lean4-p)))
      (let* ((region (lean4-info--widget-region pos))
             (min (car region))
             (max (cdr region)))
        (when (and min max)
          ;; TODO: this doesn't do the correct range.  should
          ;; store all "parents" as text properties
          (pulse-momentary-highlight-region min max)))
      (when (and lean4-info--server p)
        (jsonrpc-async-request
         lean4-info--server :$/lean/rpc/call
         `(:method "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
                   :sessionId ,lean4-info--sessionId
                   :textDocument ,lean4-info--textDocument
                   :position ,lean4-info--position
                   :params (:p ,p))
         :success-fn
         (lambda (result)
           (with-current-buffer (get-buffer-create "*DebugInfoResults*")
             (erase-buffer)
             (insert (format "result: %s" result)))
           (let* ((doc (plist-get result :doc))
                  (type (lean4-info-parse-type (plist-get result :type)
                                               nil))
                  (expr (lean4-info-parse-expr (plist-get result :exprExplicit)))
                  (expr-type (and type expr (concat expr " : " type)))
                  (sep (when (and expr-type doc) "\n")))
             (funcall cb
                      (concat expr-type sep doc)
                      :echo (concat expr-type sep (when doc (substring doc 0 (string-match "\n" doc))))))))))))



(provide 'lean4-info)
;;; lean4-info.el ends here
