;;; trackatron.el --- Music tracker for Emacs.       -*- lexical-binding: t; -*-
;;;
;;; Copyright (C) 2021  Roger Pibernat

;;; Author: Roger Pibernat <hello@rogerpibernat.com>
;;;
;;; Commentary:
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add this to your config file to use trackatro-mode with .trk files
;; (add-to-list 'auto-mode-alist '("\\.trk\\'" . trackatron-mode))
;;

;;; keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode trackatron-mode
  "Trackatron mode."
  :keymap (make-sparse-keymap)
  (evil-define-key 'normal 'trackatron-mode "d" 'tktn-delete-char)
  (evil-define-key 'normal 'trackatron-mode "x" 'tktn-delete-char)
  (add-hook 'evil-insert-state-entry-hook 'overwrite-mode nil t)
  (overwrite-mode))

(add-to-list 'auto-mode-alist '("\\.trk\\'" . trackatron-mode))

;; instead fo deleting chars replace them for original chars in the sequencer grid at that position

;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tktn-default-sequencer-steps 32)
;; (setq tktn-empty-sequencer-string (tktn-create-empty-sequencer-string tktn-default-sequencer-steps))
(setq tktn-empty-sequencer-string nil)
(setq tktn-sequencer-current-step 0)

;; Grid
(setq tktn-empty-track-step-string "\t...\s..\s......")
(setq tktn-track-header "\tNOT\sIN\sF1F2F3")
(setq tktn-empty-track nil)

(fmakunbound 'tktn-track-grid-string)

;;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trackatron ()
  "Open Trackatron."
  (tktn-make-track-window)
  )

(defun tktn-make-track-window ()
  (get-buffer-create "tracks.trk")
  (switch-to-buffer "tracks.trk")
  (insert (tktn-track-grid 64))
  (goto-char 19)
  (trackatron-mode))

(defun tktn-track-grid (steps)
  "Create an empty grid for a track of any number of STEPS."
  (concat tktn-track-header "\n" (tktn-track-grid-string steps tktn-empty-track-step-string))
  )

(defun tktn-track-grid-string (steps empty-step-string)
  "Return a grid with the given number of STEPS using the EMPTY-STEP-STRING."
  (let ((step 1)
        (grid-string ""))
    (while (<= step steps)
      (setq grid-string (format (concat  grid-string "%02X" empty-step-string "\n") step))
      (setq step (+ step 1)))
    grid-string))

(defun tktn-delete-char ()
  "Delete current character and restore grid space at this point."
  (interactive) ; to be able to call it with evil-define-key or evil-set-key
  ;; (message (substring (concat "00\t" tktn-empty-track-step-string) (+ (current-column)) (+ (current-column) 1)))
  (delete-char 1)
  (insert (substring (concat "00\t" tktn-empty-track-step-string) (+ (current-column)) (+ (current-column) 1)))
  )

;; (defun tktn-start-sequencer (tempo)
;;   (setq tktn-sequencer-timer (run-with-timer "1 sec" 1 (lambda () (trtn-sequencer-next-step)))))

;; (defun tktn-stop-sequencer ()
;;   (cancel-timer tktn-sequencer-timer))

;; (defun tktn-sequencer-next-step ()
;;   (setq tktn-sequencer-current-step (% (+ 1 tktn-sequencer-current-step) tktn-sequencer-steps))
;;   (message "%02X" tktn-sequencer-current-step))

;; (defun tktn-sequencer-next-step ()
;;   (setq atime (run-at-time "1 sec" 1 (lambda () (message "alo")))))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number-to-hex (number)
  (format "%02X" number))

;;; trackatron.el ends here
