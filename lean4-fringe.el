;;; lean4-fringe.el --- Show Lean processing progress in the editor fringe -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Authors: Gabriel Ebner, Sebastian Ullrich
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
;;
;; Show Lean processing progress in the editor fringe
;;
;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lean4-settings)
(require 'lean4-util)

(defvar-local lean4-fringe-delay-timer nil)

(defface lean4-fringe-face
  nil
  "Face to highlight Lean file progress."
  :group 'lean4)

(if (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lean4-fringe-fringe-bitmap
    (vector) 16 8))

(defface lean4-fringe-fringe-processing-face
  '((((class color) (background light))
     :background "chocolate1")
    (((class color) (background dark))
     :background "navajo white")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file processing progress."
  :group 'lean)

(defface lean4-fringe-fringe-fatal-error-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file fatal errors."
  :group 'lean)

(defun lean4-fringe-fringe-face (lean-file-progress-processing-info)
  (let ((kind (cl-getf lean-file-progress-processing-info :kind)))
    (cond
     ((eq kind 1) 'lean4-fringe-fringe-processing-face)
     (t 'lean4-fringe-fringe-fatal-error-face))))

(defvar-local lean4-fringe-data nil)

(defun lean4-fringe-update-progress-overlays ()
  "Update processing bars in the current buffer."
  (dolist (ov (flatten-tree (overlay-lists)))
    (when (eq (overlay-get ov 'face) 'lean4-fringe-face)
      (delete-overlay ov)))
  (when lean4-show-file-progress
    (seq-doseq (item lean4-fringe-data)
      (let* ((reg (eglot-range-region (cl-getf item :range)))
             (ov (make-overlay (car reg) (cdr reg))))
        (overlay-put ov 'face 'lean4-fringe-face)
        (overlay-put ov 'line-prefix
                     (propertize " " 'display
                                 `(left-fringe lean4-fringe-fringe-bitmap ,(lean4-fringe-fringe-face item))))
        (overlay-put ov 'help-echo (format "processing..."))))))

(defvar-local lean4-fringe-delay-timer nil)

(defun lean4-fringe-update (server processing uri)
  (lean4-with-uri-buffers server uri
    (setq lean4-fringe-data processing)
    (unless (and lean4-fringe-delay-timer
                 (memq lean4-fringe-delay-timer timer-list))
      (setq lean4-fringe-delay-timer
            (run-at-time 0.3 nil
                         (lambda (buf)
                           (with-current-buffer buf
                             (lean4-fringe-update-progress-overlays)
                             (setq lean4-fringe-delay-timer nil)))
                         (current-buffer))))))

(provide 'lean4-fringe)
;;; lean4-fringe.el ends here
