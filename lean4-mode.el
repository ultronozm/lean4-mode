;;; lean4-mode.el --- A major mode for the Lean language -*- lexical-binding: t -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Copyright (c) 2014, 2015 Soonho Kong. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;; Maintainer: Sebastian Ullrich <sebasti@nullri.ch>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (s "1.10.0") (f "0.19.0") (flycheck "30") (magit-section "2.90.1") (eglot "1.15") (markdown-mode "2.6"))
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

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;; See the README.md for more advanced features and the
;; associated keybindings.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'pcase)
(require 'flycheck)
(require 'markdown-mode)
(require 'eglot)
(require 'lean4-eri)
(require 'lean4-util)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-info)
(require 'lean4-dev)
(require 'lean4-fringe)
(require 'lean4-lake)

;; Silence byte-compiler
(defvar lsp--cur-version)
(defvar markdown-code-lang-modes)
(defvar compilation-mode-font-lock-keywords)
(defvar flymake-no-changes-timeout)
(defvar flymake-start-on-flymake-mode nil)
(defvar flymake-start-on-save-buffer nil)

(declare-function lean-mode "ext:lean-mode")
(declare-function flymake-proc-init-create-temp-buffer-copy "flymake-proc")
(declare-function quail-show-key "quail")

(defun lean4-compile-string (lake-name exe-name args file-name)
  "Command to run EXE-NAME with extra ARGS and FILE-NAME.
If LAKE-NAME is nonempty, then prepend \"LAKE-NAME env\" to the command
\"EXE-NAME ARGS FILE-NAME\"."
  (if lake-name
      (format "%s env %s %s %s" lake-name exe-name args file-name)
      (format "%s %s %s" exe-name args file-name)))

(defun lean4-create-temp-in-system-tempdir (file-name prefix)
  "Create a temp lean file and return its name.
The new file has prefix PREFIX (defaults to `flymake') and the same extension as
FILE-NAME."
  (make-temp-file (or prefix "flymake") nil (f-ext file-name)))

(defun lean4-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  (when (called-interactively-p 'any)
    (setq arg (read-string "arg: " arg)))
  (let* ((cc compile-command)
	 (dd default-directory)
	 (use-lake (lean4-lake-find-dir))
	 (default-directory (if use-lake (lean4-lake-find-dir) dd))
         (target-file-name
          (or
           (buffer-file-name)
           (flymake-proc-init-create-temp-buffer-copy 'lean4-create-temp-in-system-tempdir))))
    (compile (lean4-compile-string
	      (if use-lake (shell-quote-argument (f-full (lean4-get-executable lean4-lake-name))) nil)
              (shell-quote-argument (f-full (lean4-get-executable lean4-executable-name)))
              (or arg "")
              (shell-quote-argument (f-full target-file-name))))
    ;; restore old value
    (setq compile-command cc)
    (setq default-directory dd)))

(defun lean4-std-exe ()
  "Execute Lean in the current buffer."
  (interactive)
  (lean4-execute))

(defun lean4-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--managed-mode -1)
    (eglot--managed-mode)))

(defun lean4-indent-line ()
  "Lean 4 indent line function.
If point is at the end of the current indentation, use `lean4-eri-indent`;
or if point is before that position, move it there; or do nothing, to allow
tab completion (if configured)."
  (let ((cur-column (current-column))
        (cur-indent (current-indentation)))
    (cond ((= cur-column cur-indent)
           (lean4-eri-indent))
          ((< cur-column cur-indent)
           (move-to-column cur-indent)))))

(defun lean4-set-keys ()
  "Setup Lean 4 keybindings."
  (local-set-key lean4-keybinding-std-exe1                  #'lean4-std-exe)
  (local-set-key lean4-keybinding-std-exe2                  #'lean4-std-exe)
  (local-set-key lean4-keybinding-show-key                  #'quail-show-key)
  ;; (local-set-key lean4-keybinding-hole                      #'lean4-hole)
  (local-set-key lean4-keybinding-lean4-toggle-info         #'lean4-toggle-info)
  ;; (local-set-key lean4-keybinding-lean4-message-boxes-toggle #'lean4-message-boxes-toggle)
  (local-set-key lean4-keybinding-lake-build                #'lean4-lake-build)
  (local-set-key lean4-keybinding-refresh-file-dependencies #'lean4-refresh-file-dependencies)
  ;; This only works as a mouse binding due to the event, so it is not abstracted
  ;; to avoid user confusion.
  ;; (local-set-key (kbd "<mouse-3>")                         #'lean4-right-click-show-menu)
  )

(define-abbrev-table 'lean4-abbrev-table
  '())

(defvar lean4-mode-map (make-sparse-keymap)
  "Keymap used in Lean mode.")

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  `("Lean 4"
    ["Execute lean"         lean4-execute                      t]
    ["Toggle info display"  lean4-toggle-info                  t]
    ["List of errors"       flycheck-list-errors               flycheck-mode]
    ["Restart lean process" eglot-reconnect                    t]
    ["Customize lean4-mode" (customize-group 'lean)            t]))

(defvar lean4-idle-hook nil
  "Functions to run after Emacs has been idle for `lean4-idle-delay` seconds.
The functions are run only once for each time Emacs becomes idle.")

(defvar lean4--idle-timer nil)

(defun lean4--idle-function ()
  (run-hooks 'lean4-idle-hook))

(defun lean4--start-idle-timer ()
  (unless lean4--idle-timer
    (setq lean4--idle-timer
          (run-with-idle-timer lean4-idle-delay t #'lean4--idle-function))))

(defun lean4--cancel-idle-timer ()
  (when lean4--idle-timer
    (cancel-timer lean4--idle-timer)
    (setq lean4--idle-timer nil)))

(lean4--start-idle-timer)

(defconst lean4-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (before-save-hook . lean4-whitespace-cleanup)
    ;; info view
    ;; update errors immediately, but delay querying goal
    (flycheck-after-syntax-check-hook . lean4-info-buffer-redisplay-debounced)
    (post-command-hook . lean4-info-buffer-redisplay-debounced)
    (eglot--managed-mode-hook . lean4-info-buffer-redisplay-debounced)
    (lean4-idle-hook . lean4-info-buffer-refresh))
  "Hooks which lean4-mode needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

(cl-defmethod project-root ((project (head lake)))
  "A pair ('lake . DIR) is a Lean 4 project whose root directory is DIR.
This will allow us to use Emacs when a repo contains multiple lean packages."
  (cdr project))

(defun lean4-project-find (file-name)
  "Find the root directory for a Lean 4 project by searching for lakefiles."
  (when (bound-and-true-p eglot-lsp-context)
    (let (root)
      (while-let ((dir (locate-dominating-file file-name "lakefile.lean")))
        ;; We found a lakefile, but maybe it belongs to a package.
        ;; Continue looking until there are no more lakefiles.
        (setq root dir
              file-name (file-name-directory (directory-file-name dir))))
      (when root
	(cons 'lake root)))))

(push #'lean4-project-find project-find-functions)

;; Automode List
;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean.
\\{lean4-mode-map}
Invokes `lean4-mode-hook'."
  :syntax-table lean4-syntax-table
  :abbrev-table lean4-abbrev-table
  :group 'lean
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-start-skip) "[-/]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-/\\|\\s>\\)")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-defaults) lean4-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set 'compilation-mode-font-lock-keywords '())
  (require 'lean4-input)
  (set-input-method "Lean")
  (setq-local indent-line-function 'lean4-indent-line)
  ;; Inhibit flymake from starting automatically. Since diagnostics
  ;; are updated only by the language server, we call `flymake-start'
  ;; on their receipt.
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-on-flymake-mode nil)
  (setq-local flymake-start-on-save-buffer nil)
  (lean4-set-keys)
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  ;; (abbrev-mode 1)
  (pcase-dolist (`(,hook . ,fn) lean4-hooks-alist)
    (add-hook hook fn nil 'local))
  (eglot-ensure))

(defun lean4--version ()
  "Return Lean version as a list `(MAJOR MINOR PATCH)'."
  (with-temp-buffer
    (call-process (lean4-get-executable "lean") nil (list t nil) nil "-v")
    (goto-char (point-min))
    (re-search-forward (rx bol "Lean (version " (group (+ digit) (+ "." (+ digit)))))
    (version-to-list (match-string 1))))

(defun lean4-show-version ()
  "Print Lean 4 version."
  (interactive)
  (message "Lean %s" (mapconcat #'number-to-string (lean4--version) ".")))

;;;###autoload
(defun lean4-select-mode ()
  "Automatically select mode (Lean 3 vs Lean 4)."
  (if (and lean4-autodetect-lean3
           (eq 3 (car (lean4--version))))
      (lean-mode)
    (lean4-mode)))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(push '("\\.lean\\'" . lean4-select-mode) auto-mode-alist)

;;;###autoload
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("lean" . lean4-select-mode)))

;; Use utf-8 encoding
;;;### autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(defun lean4--server-cmd ()
  "Return Lean server command.
If found lake version at least 3.1.0, then return '/path/to/lake serve',
otherwise return '/path/to/lean --server'."
  (condition-case nil
      (if (string-version-lessp (car (process-lines (lean4-get-executable "lake") "--version")) "3.1.0")
          `(,(lean4-get-executable lean4-executable-name) "--server")
        `(,(lean4-get-executable "lake") "serve"))
    (error `(,(lean4-get-executable lean4-executable-name) "--server"))))

;; Eglot init
(defun lean4--server-class-init (&optional _interactive)
  (cons 'lean4-eglot-lsp-server (lean4--server-cmd)))

(push (cons 'lean4-mode #'lean4--server-class-init) eglot-server-programs)


(defclass lean4-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Eglot LSP server subclass for the Lean 4 server.")

(cl-defmethod eglot-handle-notification ((server lean4-eglot-lsp-server)
                                         (_method (eql $/lean/fileProgress))
                                         &key textDocument processing)
  "Handle notification $/lean/fileProgress."
  (eglot--dbind ((VersionedTextDocumentIdentifier) uri) textDocument
    (lean4-fringe-update server processing uri)))

(cl-defmethod eglot-handle-notification :after ((server lean4-eglot-lsp-server)
                                                (_method (eql textDocument/publishDiagnostics))
                                                &key uri &allow-other-keys)
  "Handle notification textDocument/publishDiagnostics."
  (lean4-with-uri-buffers server uri
    (lean4-info-buffer-redisplay-debounced)
    (flymake-start)))

(cl-defmethod eglot-register-capability ((_server lean4-eglot-lsp-server)
                                         (_method (eql workspace/didChangeWatchedFiles))
                                         _id &key _watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

(cl-defmethod eglot-unregister-capability ((_server lean4-eglot-lsp-server)
                                           (_method (eql workspace/didChangeWatchedFiles))
                                           _id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

;; Workarounds for code actions, see
;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66552
;;   https://github.com/leanprover/lean4/pull/2721
(defun lean4-mode--before--eglot-read-execute-code-action (args)
  "Before advice for eglot--read-execute-code-action for the Lean 4 server.
For \"Try this\" quickfixes, append the new text to the title, so the user knows
which item is which."
  (when (eq (type-of (eglot-current-server)) 'lean4-eglot-lsp-server)
    (dolist (action (car args))
      (eglot--dbind ((CodeAction) edit) action
        (when edit
          (eglot--dbind ((WorkspaceEdit) documentChanges) edit
            ;; Eglot cannot handle duplicate titles in a list of code actions
            ;; because the title is used as a key in the alist passed to
            ;; `completing-read'. To disambiguate (and incidentally, let the
            ;; user know which action is which), append the newText to the
            ;; title. Do this only if the original title is "Apply 'Try this'".
            ;; Currently only the "Try this" quickfixes from Mathlib library
            ;; search tactics (exact?, apply?, rw?) are sent as a list of code
            ;; actions with identical titles.
            (let* ((title (cl-getf action :title))
                   (change0 (aref documentChanges 0))
                   (edit0 (aref (cl-getf change0 :edits) 0))
                   (newText (cl-getf edit0 :newText)))
              (when (string= title "Apply 'Try this'")
                (setf (cl-getf action :title)
                      (concat title ": " newText)))))))))
    args)

(advice-add 'eglot--read-execute-code-action :filter-args
            #'lean4-mode--before--eglot-read-execute-code-action)

(cl-defmethod eglot-execute :before ((_server lean4-eglot-lsp-server) action)
  "Massage a `CodeAction` before Eglot handles it.
If ACTION is a fully resolved `CodeAction' (that is, if it contains edits)
and if any text document version number is zero, set it to nil to tell
Eglot not to validate the version."
  (eglot--dcase action
    (((CodeAction) edit)
     (when edit
       (eglot--dbind ((WorkspaceEdit) documentChanges) edit
         ;; Set each document version to nil if it is zero
         (mapc (eglot--lambda ((TextDocumentEdit) textDocument)
                 (when (eq (cl-getf textDocument :version) 0)
                   (setf (cl-getf textDocument :version) nil)))
               documentChanges))))))

(provide 'lean4-mode)
;;; lean4-mode.el ends here
