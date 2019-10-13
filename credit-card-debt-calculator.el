
(defun ccdc--raw-months-to-pay-off (balance apr-fraction monthly-payment)
  (let ((inc-i (+ (/ apr-fraction 365.0) 1.0))
        (b-on-p (/ balance monthly-payment)))
    (- (/ (log (+ 1.0 (* b-on-p (- 1 (expt inc-i 30.0)))))
          (* 30.0 (log inc-i))))))

(defun credit-card-compute-months-to-pay-off (balance apr monthly-payment)
  (ceiling
   (ccdc--raw-months-to-pay-off balance
                                (/ apr 100.0)
                                monthly-payment)))

(defvar credit-card-calculator-mode nil
  "Mode variable for \"Credit Card Calculator Mode\"")

(defvar credit-card-mode-hook nil
  "Hook functions for \"Credit Card Calculator Mode\"")

(defconst ccdc--mode-name "Credit Card Calculator Mode")
(defconst ccdc--buffer-name (concat "* " ccdc--mode-name " *"))

(defun ccdc--get-new-buffer ()
  (let ((old-buffer (get-buffer ccdc--buffer-name)))
    (when old-buffer
      (kill-buffer old-buffer))
    (get-buffer-create ccdc--buffer-name)))

(defun ccdc--switch-to-new-buffer ()
  (let ((buffer (ccdc--get-new-buffer)))
    (display-buffer buffer)
    (set-buffer buffer)))

(defun ccdc--mode ()
  (kill-all-local-variables)  
  (setq major-mode 'credit-card-calculator-mode)
  (setq mode-name ccdc--mode-name)
  (run-hooks 'credit-card-mode-hook))

(defun credit-card-compute-months ()
  (interactive)
  (ccdc--switch-to-new-buffer)
  (ccdc--mode))

(provide 'credit-card-debt-calculator)
