;;; consult-xref-stack --- Traverse the Xref stack.     -*-lexical-binding:t-*-

;; Copyright (C) 2024 Brett Lempereur

;; Author: Brett Lempereur <naufauna@gmail.com>

;; Homepage: https://github.com/brett-lempereur/consult-xref-stack
;; Keywords: xref

;; Package-Version: 1.0.0
;; Package-Requires: (
;;     (emacs "28.1")
;;     (consult "1.8"))

;; SPDX-License-Identifier: MIT

;; MIT License
;;
;; Copyright (c) 2024 Brett Lempereur
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice (including the next
;; paragraph) shall be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:

;; Provides Xref stack navigation integration for Consult.

;;; Code:

(require 'consult)
(require 'consult-xref)
(require 'xref)

(defun consult-xref-stack--history ()
  "Return xref history using `xref-history-storage'."
  (funcall xref-history-storage))

(defun consult-xref-stack--backward-history ()
  "Return the backward history stack."
  (car (consult-xref-stack--history)))

(defun consult-xref-stack--forward-history ()
  "Return the forward history stack."
  (cdr (consult-xref-stack--history)))

(defun consult-xref-stack--backward-jump (pos)
  "Navigate backwards to POS and update the backward and forward stacks."
  (when pos
    (when (consp pos) (setq pos (car pos)))
    ;; Update the state of the source and target stacks.
    (let ((history (consult-xref-stack--history)))
      (if (not (member pos (car history)))
          (error "Marker is not in backwards stack")
        (while (not (equal (caar history) pos))
          (pop (car history)))
        (pop (car history))
        (unless (equal (point-marker) (cadr history))
          (push (point-marker) (cdr history)))))
    ;; Jump to the selected marker, ensuring the buffer exists and any narrowed
    ;; regions are visible.
    (when (consult--jump-ensure-buffer pos)
      (unless (= (goto-char pos) (point))
        (widen)
        (goto-char pos)))
    (consult--invisible-open-permanently)
    (run-hooks 'consult-after-jump-hook))
  nil)

(defun consult-xref-stack--forward-jump (pos)
  "Navigate forwards to POS and update the backward and forward stacks."
  (when pos
    (when (consp pos) (setq pos (car pos)))
    ;; Update the state of the source and target stacks.
    (let ((history (consult-xref-stack--history)))
      (if (not (member pos (cdr history)))
          (error "Marker is not in forwards stack")
        (while (not (equal (cadr history) pos))
          (pop (cdr history)))
        (pop (cdr history))
        (unless (equal (point-marker) (caar history))
          (push (point-marker) (car history)))))
    ;; Jump to the selected marker, ensuring the buffer exists and any narrowed
    ;; regions are visible.
    (when (consult--jump-ensure-buffer pos)
      (unless (= (goto-char pos) (point))
        (widen)
        (goto-char pos)))
    (consult--invisible-open-permanently)
    (run-hooks 'consult-after-jump-hook))
  nil)

(defun consult-xref-stack--backward-state ()
  "State function used to select a candidate position in the backward stack."
  (consult--state-with-return (consult--jump-preview)
                              #'consult-xref-stack--backward-jump))

(defun consult-xref-stack--forward-state ()
  "State function used to select a candidate position in the forward stack."
  (consult--state-with-return (consult--jump-preview)
                              #'consult-xref-stack--forward-jump))

;;;###autoload
(defun consult-xref-stack-backward ()
  "Jump to a marker in the Xref backward history stack.

The command supports preview of the currently selected position."
  (interactive)
  (consult--read
   (consult--global-mark-candidates (consult-xref-stack--backward-history))
   :prompt "Go to previous cross-reference: "
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :state (consult-xref-stack--backward-state)))

;;;###autoload
(defun consult-xref-stack-forward ()
  "Jump to a marker in the Xref forward history stack.

The command supports preview of the currently selected position."
  (interactive)
  (consult--read
   (consult--global-mark-candidates (consult-xref-stack--forward-history))
   :prompt "Go to following cross-reference: "
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :state (consult-xref-stack--forward-state)))

(provide 'consult-xref-stack)
;;; consult-xref-stack.el ends here
