;;; trackatron.el --- Music tracker for Emacs.       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Roger Pibernat

;; Author: Roger Pibernat <hello@rogerpibernat.com>
;; Keywords:
;;
;; --
;;
;; This file is part of Trackatron.
;;
;; Trackatron is free software: you can redistribute it and;or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Trackatron is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Trackatron.  If not, see <https:;;www.gnu.org;licenses;>.
;;

;;; keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode trackatron-mode
  "Trackatron mode."
  :keymap (make-sparse-keymap)
  (overwrite-mode))

;; instead fo deleting chars replace them for original chars in the sequencer grid at that position
(evil-define-key 'normal 'trackatron-mode "d" 'tktn-replace-with-original-grid-char)
(evil-define-key 'normal 'trackatron-mode "x" 'tktn-replace-with-original-grid-char)

;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tktn-default-sequencer-steps 32)
;; (setq tktn-empty-sequencer-string (tktn-create-empty-sequencer-string tktn-default-sequencer-steps))
(setq tktn-empty-sequencer-string nil)
(setq tktn-sequencer-current-step 0)

;; Grid
(setq tktn-track-step-template "\t...\s..\s......")
(setq tktn-track-header "\tNOT\sIN\sF1F2F3")
(setq tktn-empty-track nil)

;;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trackatron ()
  "Open Trackatron."
  (tktn-insert-track 64)
  )

(defun tktn-insert-track (steps)
  (setq tktn-empty-track (concat tktn-track-header "\n" (tktn-track-grid steps tktn-track-step-template)))
  )

(defun tktn-track-grid (steps step-template)
  "Return a grid with the given number of STEPS using the STEP-TEMPLATE."
  (let ((step 1)
        ;; "Counter."
        (grid-string ""))
    (while (<= step steps)
      (setq grid-string (format (concat  grid-string "%02X" step-template "\n") step))
      (setq step (+ step 1)))
    grid-string))


(defun tktn-start-sequencer (tempo)
  (setq tktn-sequencer-timer (run-with-timer "1 sec" 1 (lambda () (trtn-sequencer-next-step)))))

(defun tktn-stop-sequencer ()
  (cancel-timer tktn-sequencer-timer))

(defun tktn-sequencer-next-step ()
  (setq tktn-sequencer-current-step (% (+ 1 tktn-sequencer-current-step) tktn-sequencer-steps))
  (message "%02X" tktn-sequencer-current-step))

(defun tktn-sequencer-next-step ()
  (setq atime (run-at-time "1 sec" 1 (lambda () (message "alo")))))

;; (defun foo ()
;;   (draw-sequencer)
;;   (dotimes (line 10)
;;     (setq ol (make-overlay (- (* line line-width) line-width) line))
;;     (overlay-put ol 'face "font-lock-comment-face")
;;     (setq ol (make-overlay line (+ (replace* line line-width) line-width)))
;;     (overlay-put ol 'face "font-lock-keyword-face")
;;     (sit-for 0.4)))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number-to-hex (number)
  (format "%02X" number))

;; inserts a character from the grid at the given position
(defun tktn-replace-with-original-grid-char ()
  (interactive) ; to be able to call it with evil-define-key or evil-set-key
  (message "%d" (current-column))
  (replace-string "0")
  (buffer-substring (current-column) (+ 1 (current-column))))

;;; ui.el ends here

;; testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trackatron)
;; (create-tktn-split-window)
;; (tktn-create-sequencer)
;; (tktn-display)
;; (tktn-start-sequencer 120)
;; (tktn-stop-sequencer)
;; (tktn-sequencer-next-step)
;; (tktn-paste-original-grid-char)
;; (tktn-create-empty-sequencer-string 32)

;;; trackatron.el ends here
