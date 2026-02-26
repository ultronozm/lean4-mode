;;; lean4-util.el --- Utilities for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
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

;; This library provides utilities for `lean4-mode'.

;;; Code:

(require 'cl-lib)
(require 'lean4-settings)

(defun lean4-line-offset (&optional pos)
  "Return the byte-offset of POS or current position.
Counts from the beginning of the line."
  (interactive)
  (let* ((pos (or pos (point)))
         (bol-pos
          (save-excursion
            (goto-char pos)
            (beginning-of-line)
            (point))))
    (- pos bol-pos)))

(defun lean4-pos-at-line-col (l c)
  "Return the point of the given line L and column C."
  ;; http://emacs.stackexchange.com/a/8083
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (move-to-column c)
    (point)))

(defun lean4-whitespace-cleanup ()
  "Delete trailing whitespace if `lean4-delete-trailing-whitespace' is t."
  (when lean4-delete-trailing-whitespace
    (delete-trailing-whitespace)))

(defun lean4-in-comment-p ()
  "Return t if a current point is inside of comment block.  Return nil otherwise."
  (nth 4 (syntax-ppss)))

(defvar lean4--uri-match-debug-seen (make-hash-table :test #'equal)
  "Deduplication cache for URI-mismatch diagnostics.")

(defvar lean4--uri-match-debug-count 0
  "Number of URI-mismatch diagnostics emitted in this Emacs session.")

(defun lean4-uri-match-debug-reset ()
  "Reset URI-mismatch diagnostic deduplication state."
  (interactive)
  (setq lean4--uri-match-debug-count 0)
  (clrhash lean4--uri-match-debug-seen))

(defun lean4--uri-match-debug-log-once (key format-string &rest args)
  "Log once per KEY about URI mismatch details.
FORMAT-STRING and ARGS are passed to `message'."
  (when (and lean4-debug-uri-matching
             (< lean4--uri-match-debug-count
                lean4-debug-uri-matching-max-logs)
             (not (gethash key lean4--uri-match-debug-seen)))
    (puthash key t lean4--uri-match-debug-seen)
    (setq lean4--uri-match-debug-count (1+ lean4--uri-match-debug-count))
    (apply #'message (concat "[lean4-uri] " format-string) args)))

(defmacro lean4-with-uri-buffers (server uri &rest body)
  (declare (indent 2)
           (debug (form form &rest form)))
  (let ((uri-var (make-symbol "uri"))
        (matched-var (make-symbol "matched"))
        (sample-var (make-symbol "sample"))
        (sample-count-var (make-symbol "sample-count"))
        (cache-var (make-symbol "cache"))
        (cache-hit-var (make-symbol "cache-hit"))
        (buf-uri-var (make-symbol "buf-uri"))
        (source-var (make-symbol "source")))
    `(let ((,uri-var ,uri)
           (,matched-var nil)
           (,sample-var nil)
           (,sample-count-var 0))
       (when (keywordp ,uri-var)
         (setq ,uri-var (substring (symbol-name ,uri-var) 1)))
       (dolist (buf (eglot--managed-buffers ,server))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((,cache-var (and (boundp 'eglot--TextDocumentIdentifier-cache)
                                     eglot--TextDocumentIdentifier-cache))
                    (,cache-hit-var (consp ,cache-var))
                    (,buf-uri-var (and ,cache-hit-var
                                       (plist-get (cdr ,cache-var) :uri)))
                    (,source-var "cache"))
               ;; Keep the hot notification path URI-based to avoid TRAMP
               ;; round-trips from `file-equal-p'/`file-truename'.
               (unless ,buf-uri-var
                 (setq ,source-var "fallback")
                 (when buffer-file-name
                   ;; `:truenamep t' keeps this fallback string-based for
                   ;; TRAMP paths and avoids synchronous remote stat calls.
                   (setq ,buf-uri-var (eglot-path-to-uri buffer-file-name
                                                         :truenamep t))))
               (when (and lean4-debug-uri-matching
                          (< ,sample-count-var 3))
                 (push (format "%s[%s]:%s"
                               (buffer-name buf)
                               (if ,cache-hit-var "cache" "fallback")
                               (or ,buf-uri-var "<nil>"))
                       ,sample-var)
                 (setq ,sample-count-var (1+ ,sample-count-var)))
               (when (and ,buf-uri-var
                          (equal ,buf-uri-var ,uri-var))
                 (setq ,matched-var t)
                 ,@body)))))
       (unless ,matched-var
         (lean4--uri-match-debug-log-once
          (list :no-match ,uri-var)
          "no match for uri=%s samples=%s"
          ,uri-var
          (if ,sample-var
              (mapconcat #'identity (nreverse ,sample-var) " | ")
            "<none>"))))))

(provide 'lean4-util)
;;; lean4-util.el ends here
