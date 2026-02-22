This repository is an unofficial fork of `lean4-mode`.

Installation
============

Before using this major mode, you need to [install Lean 4](https://leanprover.github.io/lean4/doc/setup.html#basic-setup).

To use `lean4-mode` in Emacs, clone the repository and add the following to your `init.el`:
```
;; You need to modify the following line
(add-to-list 'load-path "/path/to/lean4-mode")

;; Install the Emacs dependencies via your preferred package manager:
;;   eglot, magit-section, markdown-mode

(require 'lean4-mode)
```
Alternatively if you are a fan of `use-package` and `straight.el` you
can use the following:
```
(use-package lean4-mode
  :straight (lean4-mode
	            :type git
             :host github
	            :repo "<forkname>/lean4-mode" ;; replace with name of fork
	            :files ("*.el" "data")))
```
If you are an Elpaca user, a similar recipe works:
```
(use-package lean4-mode
  :elpaca (lean4-mode
           ;; Replace this with your fork.
           :host github
           :repo "<forkname>/lean4-mode"
           :files ("*.el" "data")))
```
If you are a doom-emacs user, adding the following to `packages.el` should work:
```
(package! lean4-mode :recipe
  (:host github
   :repo "<forkname>/lean4-mode"
   :files ("*.el" "data")))
```

After installation, visiting a `.lean` file should automatically load and enable `lean4-mode`.

Trying It Out
=============

If things are working correctly, you should see the word ``Lean 4`` in the
Emacs mode line when you open a file with extension `.lean`.  If you then type
```lean
#check id
```
the word ``#check`` should be underlined once the language server is running.

Settings
========

Set these with e.g. `M-x customize-variable` / `M-x customize-group RET lean4 RET`.

Some useful variables:

* `lean4-indent-offset`: indentation step (default: 2)
* `lean4-idle-delay`: refresh delay for goals/messages at point (default: 0.3)
* `lean4-info-plain`: plain goals (t) vs widget-based hover docs (nil; experimental)
* `lean4-show-file-progress`: highlight server processing progress (default: t)

Key Bindings and Commands
=========================

`lean4-mode` does not bind keys by default.  We suggest adding something like the following to your config:
```elisp
(with-eval-after-load 'lean4-mode
  (keymap-set lean4-mode-map "C-c C-k" #'quail-show-key)
  (keymap-set lean4-mode-map "C-c C-i" #'lean4-toggle-info)
  (keymap-set lean4-mode-map "C-c C-d" #'lean4-refresh-file-dependencies))
```
This yields the setup:
| Key                | Function                             |
|--------------------|--------------------------------------|
| <kbd>C-c C-k</kbd> | show keystroke for symbol            |
| <kbd>C-c C-d</kbd> | restart Lean server for current file |
| <kbd>C-c C-i</kbd> | toggle goal/messages buffer          |

Diagnostics are provided via Flymake (through Eglot). For diagnostics navigation,
bind <kbd>M-n</kbd> / <kbd>M-p</kbd> directly in `flymake-mode-map`:
```elisp
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))
```

Compiling
=========
Emacs has built-in support for running compilation commands.
You can build your project using `M-x project-compile` (`C-x p c`) and, e.g., `lake build`.
To set a default compilation command in `lean4-mode` buffers, use, e.g.,
```elisp
(add-hook 'lean4-mode-hook
          (lambda ()
            (setq-local compile-command "lake build ")))
```
To set compilation commands on a per-project basis, use a directory local variable, e.g., by putting
```elisp
((nil . ((compile-command . "lake build "))))
```
in `/path/to/project/root/.dir-locals.el`.

You can build individual files using `M-x compile` and, e.g., `lake env lean Foo.lean`.
