;;; sense-expand-region.el --- 'sense-region.el like' expand-region.el wrapper
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2012-2018 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.0.6
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org
;; Package-Requires: ((expand-region "0.11.0") (multiple-cursors "1.4.0"))

;;; Reference
;; Some code referenced from expand-region.el
;;
;; expand-region.el
;; Author: Magnar Sveen <magnars@gmail.com>

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'sense-expand-region)
;; (global-set-key (kbd "C-@") 'sense-expand-region)

;;; Commentary:
;;
;; This package is 'sense-region.el like' expand-region.el wrapper.
;;
;; (/ (+ sense-region.el expand-region.el) 2) ;=> sense-expand-region.el !!
;;
;; sense-region.el
;; http://www.taiyaki.org/elisp/sense-region/
;;
;; expand-region.el
;; https://github.com/magnars/expand-region.el

;;; Code:

;; require
(require 'expand-region)
(require 'multiple-cursors)

(add-to-list 'er/try-expand-list 'ser/mark-whole-line)
(add-to-list 'er/try-expand-list 'ser/mark-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sense-expand-region-before-start nil)
(defvar sense-expand-region-before-end nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sense-expand-region (arg)
  "Sense `er/expand-region'.  if ARG negative, contract region."
  (interactive "p")
  (if (eq last-command this-command)
      (ser/sense-expand-region arg)
    (let* ((p1 (point))
           (p2 (if (use-region-p) (mark) (point))))
      (setq sense-expand-region-before-start (min p1 p2))
      (setq sense-expand-region-before-end (max p1 p2))
      (if (not (region-active-p))
          (set-mark (point))
        (ser/sense-edit-lines sense-expand-region-before-start sense-expand-region-before-end)))))

;;;###autoload
(defun ser/sense-expand-region (arg)
  "Sense `er/expand-region'.  if ARG negative, contract region."
  (if (< arg 1)
      ;; `er/contract-region' will take care of negative and 0 arguments
      (er/contract-region (- arg))
    ;; We handle everything else

    (when (and (er--first-invocation)
               (not (use-region-p)))
      (push-mark nil t)  ;; one for keeping starting position
      (push-mark nil t)) ;; one for replace by set-mark in expansions

    (when (not (eq t transient-mark-mode))
      (setq transient-mark-mode (cons 'only transient-mark-mode)))

    (while (>= arg 1)
      (setq arg (- arg 1))
      (let* ((p1 (point))
             (p2 (if (use-region-p) (mark) (point)))
             (start (min p1 p2))
             (end (max p1 p2))
             (try-list er/try-expand-list)
             (best-start 1)
             (best-end (buffer-end 1))
             (longest-start 0)
             (longest-end 0))

        ;; add hook to clear history on buffer changes
        (unless er/history
          (add-hook 'after-change-functions 'er/clear-history t t))

        ;; remember the start and end points so we can contract later
        ;; unless we're already at maximum size
        (unless (and (= start best-start)
                     (= end best-end))
          (push (cons start end) er/history))

        (when (and (er--point-is-surrounded-by-white-space)
                   (= start end))
          (skip-chars-forward er--space-str)
          (setq start (point)))

        ;; check longest-*
        (while try-list
          (save-excursion
            (goto-char sense-expand-region-before-start)
            (set-mark sense-expand-region-before-end)
            (ignore-errors
              (funcall (car try-list))
              (when (> (- (mark) (point)) (- longest-end longest-start))
                (setq longest-start (point))
                (setq longest-end (mark))))
            (setq try-list (cdr try-list))))

        (setq try-list er/try-expand-list)

        (while try-list
          (save-excursion
            (goto-char sense-expand-region-before-start)
            (set-mark sense-expand-region-before-end)
            (setq prev-mark (mark))
            (setq prev-point (point))
            (ignore-errors
              (while (< (- (mark) (point)) (- longest-end longest-start))
                (funcall (car try-list))
                (if (and (= prev-mark (mark)) (= prev-point (point)))
                    (progn
                      (goto-char longest-start)
                      (set-mark longest-end)
                      return))

                (when (and (region-active-p)
                           (or (<= (point) start) ;; modified
                               (>= (mark) end))
                           (> (- (mark) (point)) (- end start))
                           (< (- (mark) (point)) (- best-end best-start)))
                  (setq best-start (point))
                  (setq best-end (mark))
                  (unless (minibufferp)
                    (message "%S" (car try-list))))
                (setq prev-mark (mark))
                (setq prev-point (point)))))
          (setq try-list (cdr try-list)))

        (goto-char best-start)
        (set-mark best-end)

        (when (and (= best-start longest-start)
                   (= best-end longest-end))
          (setq sense-expand-region-before-start best-start)
          (setq sense-expand-region-before-end best-end))

        (when (and (= best-start 0)
                   (= best-end (buffer-end 1))) ;; We didn't find anything new, so exit early
          (setq arg 0))))))

;;;###autoload
(defun ser/sense-edit-lines (s e)
  "Edit multiple lines.
If S `current-column` = E `current-column`, call `mc/edit-lines`.
If S `current-column` != E `current-column`, call `rectangle-mark-mode`."
  (if (eq (save-excursion (goto-char s) (current-column))
          (save-excursion (goto-char e) (current-column)))
      (call-interactively 'mc/edit-lines)
    (call-interactively 'rectangle-mark-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ser/mark-whole-line ()
  "Mark whole-line."
  (interactive)
  (set-mark (save-excursion
              (end-of-line)
              (point)))
  (back-to-indentation))

(defun ser/mark-forward ()
  "Mark forward."
  (interactive)
  (let (line-end)
    (save-excursion
      (end-of-line)
      (setq line-end (point)))
    (set-mark (save-excursion
                (unless (not (region-active-p))
                  (goto-char (region-end))
                  (goto-char (+ (point) 1)))
                (re-search-forward "[\s(\s)\>]" nil t)
                (goto-char (- (point) 1))
                (unless (> line-end (point))
                  (goto-char line-end))
                (point)))))

(provide 'sense-expand-region)
;;; sense-expand-region.el ends here
