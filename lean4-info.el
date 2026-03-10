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
(require 'subr-x)
(require 'lean4-syntax)
(require 'lean4-settings)
(require 'lean4-util)
(require 'eglot)
(require 'magit-section)
(require 'xref)

(defgroup lean4-info nil
  "Lean Info."
  :group 'lean4)

(defvar lean4-info-widget-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'lean4-info-find-definition-mouse)
    (define-key map [mouse-2] #'lean4-info-find-definition-mouse)
    map)
  "Keymap for interactive terms in the *Lean Goal* buffer.")

;; Lean Info Mode (for "*lean4-info*" buffer)
;; Automode List
;;;###autoload
(define-derived-mode lean4-info-mode prog-mode "Lean-Info"
  "Helper mode for Lean 4 info buffer.
This mode is only used in temporary buffers, for fontification."
  :syntax-table lean4-syntax-table
  :group 'lean
  (set (make-local-variable 'font-lock-defaults) lean4-info-font-lock-defaults))

(declare-function lean4--idle-invalidate "lean4-mode")
(declare-function eglot--xref-make-match "eglot" (name uri range))

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
      (eldoc-mode)
      (set-input-method "Lean")
      (set-syntax-table lean4-syntax-table)
      (setq-local font-lock-defaults lean4-info-font-lock-defaults)
      (font-lock-mode 1)
      (local-set-key [remap xref-find-definitions] #'lean4-info-find-definition)
      (setq buffer-read-only t))))

(defun lean4-toggle-info-buffer (buffer)
  "Create or delete BUFFER.
The buffer is supposed to be the *Lean Goal* buffer."
  (let ((window (get-buffer-window buffer)))
    (if window
        (quit-window nil window)
      (lean4-ensure-info-buffer buffer)
      (display-buffer buffer))))

(defcustom lean4-info-refresh-even-if-invisible nil
  "If non-nil, refresh the info buffer even if it is not visible."
  :type 'boolean)

(defun lean4-info-buffer-active (buffer)
  "Check whether the given info BUFFER should show info for the current buffer."
  (and
   (if lean4-info-refresh-even-if-invisible
       ;; info buffer exists
       (get-buffer buffer)
     ;; info buffer visible (on any frame)
     (get-buffer-window buffer t))
   (get-buffer buffer)
   ;; current window of current buffer is selected (i.e., in focus)
   (eq (current-buffer) (window-buffer))
   ;; current buffer is visiting a file
   buffer-file-name))

(defconst lean4-info-buffer-name "*Lean Goal*")

(defvar lean4-info--goals nil)
(defvar lean4-info--term-goal nil)

(defvar-local lean4-info--refresh-generation 0
  "Monotonic counter for asynchronous info-buffer refresh requests.")

(defun lean4-info--diagnostics ()
  (nreverse
   (cl-loop for diag in (flymake-diagnostics)
            when (cdr (assoc 'eglot-lsp-diag (flymake-diagnostic-data diag)))
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

(defun lean4-info--decorate-widgets ()
  "Add navigation affordances to interactive goal terms."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'lean4-p nil (point-max))))
        (when (get-text-property pos 'lean4-p)
          (let ((map (make-composed-keymap
                      lean4-info-widget-map
                      (get-text-property pos 'keymap))))
            (add-text-properties
             pos next
             `(follow-link t
               help-echo "mouse-1, mouse-2 or M-.: go to definition"
               keymap ,map
               mouse-face highlight))))
        (setq pos next)))))

(defun lean4-info--rpc-ref-at-point (&optional pos)
  "Return the Lean RPC ref at POS, or nil if there is none.
POS defaults to point."
  (car (get-text-property (or pos (point)) 'lean4-p)))

(defun lean4-info--require-rpc-ref (&optional pos)
  "Return the Lean RPC ref at POS or signal a user-facing error."
  (or (lean4-info--rpc-ref-at-point pos)
      (if lean4-info-plain
          (user-error
           "Interactive goal terms are unavailable while `lean4-info-plain' is non-nil")
        (user-error "No Lean term at point"))))

(defun lean4-info--widget-summary (&optional pos)
  "Return a compact summary of the interactive term at POS."
  (if-let* ((region (lean4-info--widget-region pos))
            (text (buffer-substring-no-properties (car region) (cdr region))))
      (replace-regexp-in-string "[[:space:]\n]+" " " (string-trim text))
    "Lean definition"))

(defun lean4-info--ensure-rpc-session ()
  "Ensure that the info buffer has an RPC session available."
  (unless (and lean4--rpc-server
               lean4--rpc-sessionId
               lean4--rpc-textDocument
               lean4--rpc-position)
    (user-error "No Lean RPC session is available for the goal buffer")))

(defun lean4-info--goto-request-params (ref kind)
  "Build RPC params for go-to KIND using RPC REF."
  `(:method "Lean.Widget.getGoToLocation"
            :sessionId ,lean4--rpc-sessionId
            :textDocument ,lean4--rpc-textDocument
            :position ,lean4--rpc-position
            :params (:kind ,kind
                     :info (:p ,ref))))

(defun lean4-info--location-uri (location)
  "Extract the target URI from LOCATION."
  (or (cl-getf location :targetUri)
      (cl-getf location :uri)))

(defun lean4-info--location-range (location)
  "Extract the best available target range from LOCATION."
  (or (cl-getf location :targetSelectionRange)
      (cl-getf location :targetRange)
      (cl-getf location :range)))

(defun lean4-info--xref-for-location (summary location)
  "Convert a Lean LOCATION to an xref using SUMMARY."
  (when-let* ((uri (lean4-info--location-uri location))
              (range (lean4-info--location-range location)))
    (cl-letf (((symbol-function 'eglot--current-server-or-lose)
               (lambda () lean4--rpc-server)))
      (eglot--xref-make-match summary uri range))))

(defun lean4-info-find-definition ()
  "Jump to the definition of the interactive goal term at point."
  (interactive)
  (let* ((origin-buffer (current-buffer))
         (origin-window (selected-window))
         (origin-point (point))
         (ref (lean4-info--require-rpc-ref origin-point))
         (summary (lean4-info--widget-summary origin-point)))
    (lean4-info--ensure-rpc-session)
    (jsonrpc-async-request
     lean4--rpc-server :$/lean/rpc/call
     (lean4-info--goto-request-params ref "definition")
     :success-fn
     (lambda (result)
       (when (buffer-live-p origin-buffer)
         (let ((xrefs (delq nil
                            (mapcar (lambda (location)
                                      (lean4-info--xref-for-location summary location))
                                    (append result nil)))))
           (if xrefs
               (if (window-live-p origin-window)
                   (with-selected-window origin-window
                     (with-current-buffer origin-buffer
                       (goto-char (min origin-point (point-max)))
                       (xref--show-defs xrefs nil)))
                 (with-current-buffer origin-buffer
                   (goto-char (min origin-point (point-max)))
                   (xref--show-defs xrefs nil)))
             (message "No Lean definition found for %s" summary)))))
     :error-fn
     (lambda (&rest _)
       (message "Lean go-to-definition request failed")))))

(defun lean4-info-find-definition-mouse (event)
  "Visit the Lean definition for the interactive goal term clicked at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (lean4-info-find-definition))

(defun lean4-mk-message-section (value caption errors)
  "Add a section with caption CAPTION and contents ERRORS."
  (when errors
    (magit-insert-section (magit-section value)
      (magit-insert-heading caption)
      (magit-insert-section-body
        (dolist (e errors)
          (magit-insert-section (magit-section)
            (magit-insert-section-body
              (eglot--dbind ((Diagnostic) message range) e
                (eglot--dbind ((Range) start) range
                  (eglot--dbind ((Position) line character) start
                    (magit-insert-heading (format "%d:%d" (1+ line) character))
                    (insert message "\n")))))))))))

(defmacro lean4-info--split (head tail predicate)
  (declare (indent 2)
           (debug (symbolp symbolp form)))
  (let ((previous (make-symbol "previous")))
    `(let (,previous)
       (setq ,tail ,head)
       (while (and ,tail (funcall ,predicate (car ,tail)))
         (setq ,previous ,tail
               ,tail (cdr ,tail)))
       (if ,previous
           (setcdr ,previous nil)
         (setq ,head nil)))))

(defun lean4-info-buffer-redisplay ()
  (let ((inhibit-message t)
        (inhibit-read-only t))
    (when (lean4-info-buffer-active lean4-info-buffer-name)
      (let ((deactivate-mark)           ; keep transient mark
            (line (save-restriction (widen) (1- (line-number-at-pos nil t))))
            (goals lean4-info--goals)
            (term-goal lean4-info--term-goal)
            (errors-above (sort (lean4-info--diagnostics)
                                (lambda (a b)
                                  (< (lean4-info--diagnostic-end a)
                                     (lean4-info--diagnostic-end b)))))
            errors-here errors-below)
        (lean4-info--split errors-above errors-here
                           (lambda (e) (< (lean4-info--diagnostic-end e) line)))
        (lean4-info--split errors-here errors-below
                           (lambda (e) (<= (lean4-info--diagnostic-start e) line)))
        (with-current-buffer lean4-info-buffer-name
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (when goals
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (magit-insert-section-body
                  (if (> (length goals) 0)
                      (seq-doseq (g goals)
                        (magit-insert-section (magit-section)
                          (insert (lean4-info--fontify-string g) "\n\n")))
                    (insert "goals accomplished\n\n")))))
            (when term-goal
              (magit-insert-section (magit-section 'term-goal)
                (magit-insert-heading "Expected type:")
                (magit-insert-section-body
                  (insert (lean4-info--fontify-string term-goal) "\n\n"))))
            (lean4-mk-message-section 'errors-here "Messages here:" errors-here)
            (lean4-mk-message-section 'errors-below "Messages below:" errors-below)
            (lean4-mk-message-section 'errors-above "Messages above:" errors-above)
            (when lean4-highlight-inaccessible-names
              (goto-char 1)
              (save-match-data
                (while (re-search-forward "\\(\\sw+\\)✝\\([¹²³⁴-⁹⁰]*\\)" nil t)
                  (replace-match
                   (propertize (concat (match-string-no-properties 1)
                                       (match-string-no-properties 2))
                               'font-lock-face 'font-lock-comment-face)
                   'fixedcase 'literal))))
            (lean4-info--decorate-widgets)
            (font-lock-ensure)))))))

(defcustom lean4-info-plain t
  "If t, then use plain text for info buffer.
If nil, then enable widget-based goal terms with hover docs and
goal-buffer go-to-definition.  This is an experimental feature
that requires further testing."
  :type
  '(choice
    (const :tag "Plain text" t)
    (const :tag "Hover docs" nil)))

(defvar lean4--rpc-server nil)
(defvar lean4--rpc-textDocument nil)
(defvar lean4--rpc-position nil)
(defvar lean4--rpc-sessionId nil)
(defvar lean4--rpc-timer nil)

(defun lean4--rpc-connect (&optional buf)
  "Initiate an rpc connection.
This sets the variables lean4--rpc-*."
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (let*
        ((server (eglot-current-server))
         (textdoc-pos (eglot--TextDocumentPositionParams))
         (uri (plist-get (plist-get textdoc-pos :textDocument) :uri))
         (response (jsonrpc-request server :$/lean/rpc/connect `(:uri ,uri)))
         (sessionId (plist-get response :sessionId)))
      (setq lean4--rpc-server server)
      (setq lean4--rpc-textDocument (plist-get textdoc-pos :textDocument))
      (setq lean4--rpc-position (plist-get textdoc-pos :position))
      (setq lean4--rpc-sessionId sessionId)
      (unless lean4--rpc-timer
        (setq lean4--rpc-timer
              (run-with-timer 0 5 #'lean4-info--rpc-keepalive))))))

(defun lean4-info--rpc-keepalive ()
  (when lean4--rpc-server
    (condition-case nil
        (jsonrpc-notify lean4--rpc-server :$/lean/rpc/keepAlive
                        `(:uri ,(plist-get lean4--rpc-textDocument :uri)
                               :sessionId ,lean4--rpc-sessionId))
      (error (cancel-timer lean4--rpc-timer)
             (setq lean4--rpc-timer nil)
             (setq lean4--rpc-server nil)
             (setq lean4--rpc-textDocument nil)
             (setq lean4--rpc-position nil)
             (setq lean4--rpc-sessionId nil)))))

(defun lean4-info-parse-goal (goal)
  "Parse GOAL into propertized string."
  (let* ((userName (plist-get goal :userName))
         (type (plist-get goal :type))
         (hyps (plist-get goal :hyps))
         (goalPrefix (plist-get goal :goalPrefix))
         (ctx (plist-get goal :ctx))
         (p (plist-get ctx :p)))
    (concat
     (when userName (concat "case " userName "\n"))
     (mapconcat (lambda (hyp)
                  (lean4-info-parse-hyp hyp (list p)))
                hyps "\n")
     "\n"
     goalPrefix
     (lean4-info-parse-type type (list p)))))

(defun lean4-info-parse-hyp (hyp ps)
  "Parse hypothesis HYP into propertized string.
PS is a list of tag IDs."
  (let* ((type (plist-get hyp :type))
         (names (plist-get hyp :names)))
    (concat (mapconcat #'identity names
                       (propertize " " 'lean4-p ps))
            (propertize " : " 'lean4-p ps)
            (lean4-info-parse-type type ps))))

(defun lean4-info-parse-type (type ps)
  "Parse TYPE into propertized string.
PS is a list of tag IDs."
  (let* ((tag (plist-get type :tag)))
    (lean4-info-parse-tag tag ps)))

(defun lean4-info-parse-tag (tag ps)
  "Parse TAG into propertized string.
PS is a list of tag IDs."
  (let* ((tag0 (aref tag 0))
         (info (plist-get tag0 :info))
         (p (plist-get info :p))
         (tag1 (aref tag 1)))
    (lean4-info-parse-expr tag1 (cons p ps))))

(defun lean4-info-parse-expr (expr &optional ps)
  "Parse EXPR into propertized string.
PS is a list of tag IDs."
  (cond
   ((equal (car expr) :text)
    (if ps
        (propertize (cadr expr) 'lean4-p ps)
      (cadr expr)))
   ((equal (car expr) :append)
    (mapconcat (lambda (item)
                 (cond
                  ((equal (car item) :text)
                   (if ps
                       (propertize (cadr item) 'lean4-p ps)
                     (cadr item)))
                  ((equal (car item) :tag)
                   (lean4-info-parse-tag (cadr item) ps))))
               (cadr expr)))))

(defun lean4-info-buffer-refresh ()
  "Refresh the *Lean Goal* buffer."
  ;; Important for TRAMP responsiveness: avoid calling `eglot-current-server'
  ;; unless the info buffer is actually active.
  (when-let* (((lean4-info-buffer-active lean4-info-buffer-name))
              (server (eglot-current-server)))
    (let* ((buf (current-buffer))
           (generation (cl-incf lean4-info--refresh-generation))
           (goals :pending)
           (term-goal :pending)
           (old-goals lean4-info--goals)
           (old-term-goal lean4-info--term-goal)
           (commit
            (lambda ()
              (when (and (not (eq goals :pending))
                         (not (eq term-goal :pending))
                         (buffer-live-p buf))
                (with-current-buffer buf
                  (when (eql generation lean4-info--refresh-generation)
                    (setq lean4-info--goals goals)
                    (setq lean4-info--term-goal term-goal)
                    (lean4-info-buffer-redisplay))))))
           (store-goals
            (lambda (value)
              (setq goals value)
              (funcall commit)))
           (store-term-goal
            (lambda (value)
              (setq term-goal value)
              (funcall commit))))
      (when server
        (if lean4-info-plain
            (progn
              (jsonrpc-async-request
               server :$/lean/plainGoal (eglot--TextDocumentPositionParams)
               :success-fn (lambda (result)
                             (funcall store-goals (cl-getf result :goals)))
               :error-fn (lambda (&rest _)
                           (funcall store-goals old-goals)))
              (jsonrpc-async-request
               server :$/lean/plainTermGoal (eglot--TextDocumentPositionParams)
               :success-fn (lambda (result)
                             (funcall store-term-goal (cl-getf result :goal)))
               :error-fn (lambda (&rest _)
                           (funcall store-term-goal old-term-goal))))
          ;; It might be more elegant to do the following once, when we
          ;; switch to a lean buffer, but putting it here seems more
          ;; robust.
          (condition-case nil
              (progn
                (lean4--rpc-connect) ;; sets the variables lean4--rpc-*
                (jsonrpc-async-request
                 server :$/lean/rpc/call
                 `(:method "Lean.Widget.getInteractiveGoals"
                           :sessionId ,lean4--rpc-sessionId
                           :textDocument ,lean4--rpc-textDocument
                           :position ,lean4--rpc-position
                           :params (:textDocument ,lean4--rpc-textDocument
                                                  :position ,lean4--rpc-position
                                                  ))
                 :success-fn
                 (lambda (result)
                   (funcall store-goals
                            (when result
                              (vconcat (mapcar #'lean4-info-parse-goal
                                               (cl-getf result :goals))))))
                 :error-fn
                 (lambda (&rest _)
                   (funcall store-goals old-goals)))
                (jsonrpc-async-request
                 server :$/lean/rpc/call
                 `(:method "Lean.Widget.getInteractiveTermGoal"
                           :sessionId ,lean4--rpc-sessionId
                           :textDocument ,lean4--rpc-textDocument
                           :position ,lean4--rpc-position
                           :params (:textDocument ,lean4--rpc-textDocument
                                                  :position ,lean4--rpc-position
                                                  ))
                 :success-fn
                 (lambda (result)
                   (funcall store-term-goal
                            (when result (lean4-info-parse-goal result))))
                 :error-fn
                 (lambda (&rest _)
                   (funcall store-term-goal old-term-goal))))
            (error
             (funcall store-goals old-goals)
             (funcall store-term-goal old-term-goal))))))))

(defun lean4-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (lean4-toggle-info-buffer lean4-info-buffer-name)
  (lean4-info-buffer-refresh))

(defun lean4-info--widget-region (&optional pos)
  "Return the region of the widget at POS.
POS defaults to the current point."
  (unless pos (setq pos (point)))
  (let ((ps (get-text-property pos 'lean4-p)))
    (when ps
      (let ((p (car ps))
            (start (point-min))
            (end (point-max)))
        (while (and start
                    (< start (point-max))
                    (not (member p (get-text-property start 'lean4-p))))
          (setq start (next-single-property-change start 'lean4-p nil (point-max))))
        (while (and end
                    (> end (point-min))
                    (not (member p (get-text-property end 'lean4-p))))
          (setq end (previous-single-property-change end 'lean4-p nil (point-min))))
        (setq end (next-single-property-change end 'lean4-p nil (point-max)))
        (cons start end)))))

(defun lean4-info-eldoc-function (cb)
  "Eldoc function for info buffer.
CB is the callback function provided by Eldoc."
  (unless lean4-info-plain
    (let* ((pos (point))
           (p (car (get-text-property pos 'lean4-p))))
      (let* ((region (lean4-info--widget-region pos))
             (min (car region))
             (max (cdr region)))
        (when (and min max)
          (pulse-momentary-highlight-region min max)))
      (when (and lean4--rpc-server p)
        (jsonrpc-async-request
         lean4--rpc-server :$/lean/rpc/call
         `(:method "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
                   :sessionId ,lean4--rpc-sessionId
                   :textDocument ,lean4--rpc-textDocument
                   :position ,lean4--rpc-position
                   :params (:p ,p))
         :success-fn
         (lambda (result)
           (let* ((doc (plist-get result :doc))
                  (type (lean4-info-parse-type (plist-get result :type)
                                               nil))
                  (expr (lean4-info-parse-expr (plist-get result :exprExplicit)))
                  (expr-type (and type expr (concat expr " : " type)))
                  (sep (when (and expr-type doc)
                         "\n")))
             (funcall cb
                      (concat expr-type sep doc)
                      :echo (concat expr-type sep
                                    (when doc
                                      (substring doc 0 (string-match-p "\n" doc))))))))))))

(provide 'lean4-info)
;;; lean4-info.el ends here
