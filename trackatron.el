;;;
;;;
;;;
;;;
;;;
;;;
;; sequencer buffer place-holder
;; (setq trackatron-sequencer-buffer nil)
(setq tktn-sequencer-header-string "\tNT O IN A F1 F2 F3")
(setq tktn-sequencer-step-string ".. . .. . .. .. ..")
(setq tktn-default-sequencer-steps 32)
(setq tktn-sequencer-current-step 0)
(setq tktn-sequencer-steps tktn-default-sequencer-steps)

(defun trackatron ()
  "Open Trackatron."
  (tktn-create-sequencer)
  )

(defun create-tktn-split-window ()
  "Create a Trackatron window in a vertical split."
  (setq buf (get-buffer-create "trackatron"))
  (split-window-horizontally)
  (set-window-buffer (next-window) "trackatron"))

(defun tktn-create-sequencer ()
  "Create a Trackatron buffer and switch to it in current window."
  (setq tktn-sequencer-buffer (get-buffer-create "tktn-sequencer"))
  (set-buffer "tktn-sequencer")
  (draw-sequencer tktn-default-sequencer-steps)
  ;; replace-text instead of inserting text
  (add-hook 'evil-insert-state-entry-hook 'overwrite-mode nil t)
  )

(defun tktn-display()
  (switch-to-buffer "tktn-sequencer"))

(defun draw-sequencer (steps)
  "Draw the sequencer."
  (erase-buffer)
  (insert tktn-sequencer-header-string)
  (newline)
  (dotimes (x steps)
    (insert (number-to-hex (+ 1 x)))
    (insert "\t" tktn-sequencer-step-string)
    (newline)))

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
;;     (setq ol (make-overlay line (+ (* line line-width) line-width)))
;;     (overlay-put ol 'face "font-lock-keyword-face")
;;     (sit-for 0.4)))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number-to-hex (number)
  (format "%02X" number))

;; testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trackatron)
;; (create-tktn-split-window)
;; (tktn-create-sequencer)
;; (tktn-display)
;; (tktn-start-sequencer 120)
;; (tktn-stop-sequencer)
;; (tktn-sequencer-next-step)

;;; ui.el ends here
