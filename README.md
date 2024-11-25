# Consult Xref Stack

Navigate the Xref stack with Consult.

## Commands

* `consult-xref-stack-backward` -- Navigate backwards through history.
* `consult-xref-stack-forward` -- Navigate forwards through history.

## Installation

Since the forward navigation commands have limited uses, you can get most of
the functionality that you probably need by just finding the backward
navigation command:

```emacs-lisp
(use-package consult-xref-stack
  :vc
  (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main")
  :bind
  (("C-," . consult-xref-stack-backward)))
```

## Dependencies

* [Consult](https://github.com/minad/consult)
