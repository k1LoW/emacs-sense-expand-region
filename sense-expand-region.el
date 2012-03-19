;;;sense-expand-region.el --- 'sense-region.el like' expand-region.el wrapper
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2012 by 101000code/101000LAB

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

;; Version: 0.0.1
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org

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
;; (/ (+ sense-region.el expand-region.el inline-string-rectangle) 5) ;=> sense-expand-region.el !!
;;
;; sense-region.el
;; http://www.taiyaki.org/elisp/sense-region/
;;
;; expand-region.el
;; https://github.com/magnars/expand-region.el

;;; Require
(require 'expand-region)
(require 'inline-string-rectangle)

(transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sense-expand-region-status nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sense-expand-region (arg)
  (interactive "p")
  (setq sense-expand-region-status nil)
  (if (eq last-command this-command)
      (er/expand-region arg)
    (if (not (region-active-p))
        (set-mark (point))
      (setq sense-expand-region-status 'rectangle)
      (inline-string-rectangle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice kill-ring-save (around sense-expand-region-kill-ring-save disable)
  (if (or (not sense-expand-region-status) (not mm/mirrors))
      ad-do-it
    (sense-expand-region-kill-new))
    (setq sense-expand-region-status nil))
(ad-enable-advice 'kill-ring-save 'around 'sense-expand-region-kill-ring-save)
(ad-activate 'kill-ring-save)

(defadvice kill-region (around sense-expand-region-kill-region disable)
  (unless (or (not sense-expand-region-status) (not mm/mirrors))
    (sense-expand-region-kill-new))
  ad-do-it
    (setq sense-expand-region-status nil))
(ad-enable-advice 'kill-region 'around 'sense-expand-region-kill-region)
(ad-activate 'kill-region)

(defun sense-expand-region-kill-new ()
  (let ((killed-text "") (mirrors mm/mirrors) (first t))
    (if (> (overlay-start (car mirrors)) (overlay-start mm/master))
        (setq mirrors (append (list mm/master) (reverse mirrors)))
      (setq mirrors (append mirrors (list mm/master))))
      (dolist (mirror mirrors)
        (setq killed-text (concat
                           killed-text
                           (unless first "\n")
                           (buffer-substring (overlay-start mirror) (overlay-end mirror))))
        (setq first nil))
      (kill-new killed-text)
      (mm/deactivate-region-and-clear-all)))

(provide 'sense-expand-region)
