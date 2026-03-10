;;; lean4-info-test.el --- Tests for Lean goal buffer helpers -*- lexical-binding: t -*-

;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;;; Commentary:

;; Focused tests for the goal-buffer interaction layer in `lean4-info'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lean4-info)

(ert-deftest lean4-info--decorate-widgets-makes-terms-clickable ()
  (with-temp-buffer
    (insert "foo " (propertize "bar" 'lean4-p '(17)) " baz")
    (lean4-info--decorate-widgets)
    (goto-char 5)
    (should (eq (get-text-property (point) 'mouse-face) 'highlight))
    (should (equal (get-text-property (point) 'help-echo)
                   "mouse-1, mouse-2 or M-.: go to definition"))
    (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-1])
                'lean4-info-find-definition-mouse))))

(ert-deftest lean4-info-find-definition-explains-plain-mode-limit ()
  (with-temp-buffer
    (setq-local lean4-info-plain t)
    (insert "plain goal text")
    (goto-char (point-min))
    (let ((err (should-error (lean4-info-find-definition) :type 'user-error)))
      (should (eq (car err) 'user-error))
      (should (string-match-p "lean4-info-plain" (cadr err))))))

(ert-deftest lean4-info-find-definition-requests-go-to-location ()
  (with-temp-buffer
    (setq lean4-info-plain nil
          lean4--rpc-server 'fake-server
          lean4--rpc-sessionId 7
          lean4--rpc-textDocument '(:uri "file:///tmp/Test.lean")
          lean4--rpc-position '(:line 3 :character 5))
    (insert (propertize "Nat.add" 'lean4-p '(23)))
    (goto-char (point-min))
    (let (request-method request-params shown-xrefs)
      (cl-letf (((symbol-function 'jsonrpc-async-request)
                 (lambda (_server method params &rest args)
                   (setq request-method method
                         request-params params)
                   (funcall
                    (plist-get args :success-fn)
                    [(:targetUri "file:///tmp/Target.lean"
                      :targetSelectionRange
                      (:start (:line 4 :character 2)
                       :end (:line 4 :character 5))
                      :targetRange
                      (:start (:line 4 :character 0)
                       :end (:line 4 :character 10)))])))
                ((symbol-function 'eglot--xref-make-match)
                 (lambda (summary uri range)
                   (list :summary summary :uri uri :range range)))
                ((symbol-function 'xref--show-defs)
                 (lambda (xrefs _display-action)
                   (setq shown-xrefs xrefs))))
        (lean4-info-find-definition)
        (should (eq request-method :$/lean/rpc/call))
        (should (equal request-params
                       '(:method "Lean.Widget.getGoToLocation"
                         :sessionId 7
                         :textDocument (:uri "file:///tmp/Test.lean")
                         :position (:line 3 :character 5)
                         :params (:kind "definition"
                                  :info (:p 23)))))
        (should (equal shown-xrefs
                       '((:summary "Nat.add"
                          :uri "file:///tmp/Target.lean"
                          :range (:start (:line 4 :character 2)
                                  :end (:line 4 :character 5))))))))))

(provide 'lean4-info-test)
;;; lean4-info-test.el ends here
