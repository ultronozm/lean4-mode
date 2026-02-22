;;; lean4-settings.el --- Custom variables for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
;; SPDX-License-Identifier: Apache-2.0
;;

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

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defgroup lean4 nil
  "Lean 4 programming language and theorem prover."
  :prefix "lean4-"
  :group 'languages
  :link '(url-link :tag "Website" "http://leanprover.github.io")
  :link '(url-link :tag "Github"  "https://github.com/leanprover/lean4"))

(defcustom lean4-delete-trailing-whitespace nil
  "Automatically delete trailing shitespace.
Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :type 'boolean)

(defcustom lean4-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.
Set this variable to t to highlight inaccessible names in the info display
using `font-lock-comment-face' instead of the `✝` suffix used by Lean."
  :type 'boolean)

(defcustom lean4-show-file-progress t
  "Highlight file progress in the current buffer."
  :type 'boolean)


(defcustom lean4-autodetect-lean3 nil
  "Autodetect Lean version.
Use elan to check if current project uses Lean 3 or Lean 4 and initialize the
right mode when visiting a file.  If elan has a default Lean version, Lean files
outside a project will default to that mode."
  :type 'boolean)

(defcustom lean4-auto-start-eglot t
  "Automatically start Eglot when entering `lean4-mode'.
If nil, Eglot is not started automatically; use `eglot' or `eglot-ensure'
to start it manually."
  :type 'boolean)

(defcustom lean4-idle-delay 0.3
  "Interval for `lean4-idle-hook` functions."
  :type 'number)

(defcustom lean4-enable-file-watchers nil
  "Honour requests from the server to watch for file modifications.
This is disabled by default because the server wants to watch \"**/*.ilean\",
and in many cases there are too many directories to watch each individually."
  :type 'boolean)

(provide 'lean4-settings)
;;; lean4-settings.el ends here
