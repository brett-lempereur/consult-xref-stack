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

(defconst consult-xref-stack--narrow
  `((?f . "Forward")
    (?b . "Backward"))
  "Xref stack narrowing configuration.")

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
          ;; Shift the backward history to the forward history.
          (push (pop (car history)) (cdr history)))
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
          ;; Shift the forwards history to the backward history.
          (push (pop (cdr history)) (car history)))
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

(defun consult-xref-stack--jump (pos)
  "Navigate backwards or forwards to POS and update the backward and
forward stacks."
  (when pos
    (when (consp pos) (setq pos (car pos)))
    (let ((history (consult-xref-stack--history)))
      ;; Order is important if the selected candidate is present in
      ;; both stacks, because at the moment we don't know whether it
      ;; came from the backward or forward group.  Forward history is
      ;; much shorter and truncated after every use of
      ;; `xref-find-definitions' , so lets start there.
      (if (member pos (cdr history))
          (consult-xref-stack--forward-jump pos)
        (consult-xref-stack--backward-jump pos))))
  nil)

(defun consult-xref-stack--backward-state ()
  "State function used to select a candidate position in the backward stack."
  (consult--state-with-return (consult--jump-preview)
                              #'consult-xref-stack--backward-jump))

(defun consult-xref-stack--forward-state ()
  "State function used to select a candidate position in the forward stack."
  (consult--state-with-return (consult--jump-preview)
                              #'consult-xref-stack--forward-jump))

(defun consult-xref-stack--state ()
  "State function used to select a candidate position in the forward or the
backward stack."
  (consult--state-with-return (consult--jump-preview)
                              #'consult-xref-stack--jump))

(defun consult-xref-stack--add-group (cands group)
  "Add text property `consult--type' with value GROUP to CANDS, to
distinguish forward and backward xref history."
  (mapcar (lambda (cand)
            (add-text-properties 0 1 `(consult--type ,group) cand)
            cand)
          cands))

(defun consult-xref-stack--candidates ()
  "Return list of candidates strings for forward and backward xref history
together."
  (mapcan (lambda (pair)
            (when-let* ((markers (car pair))
                        (direction (cdr pair)))
              (consult-xref-stack--add-group
               (consult--global-mark-candidates markers)
               direction)))
          (list (cons (consult-xref-stack--backward-history)
                      (car (rassoc "Backward" consult-xref-stack--narrow)))
                (cons (consult-xref-stack--forward-history)
                      (car (rassoc "Forward" consult-xref-stack--narrow))))))

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

;;;###autoload
(defun consult-xref-stack ()
  "Jump to a marker in the Xref history stack in both directions.

The command supports preview of the currently selected position, groups
and narrowing."
  (interactive)
  (consult--read
   (consult-xref-stack--candidates)
   :prompt "Go to previous cross-reference: "
   :category 'consult-location
   :sort nil
   :require-match t
   :group (consult--type-group consult-xref-stack--narrow)
   :narrow (consult--type-narrow consult-xref-stack--narrow)
   :lookup #'consult--lookup-location
   :state (consult-xref-stack--state)))

(provide 'consult-xref-stack)
;;; consult-xref-stack.el ends here
