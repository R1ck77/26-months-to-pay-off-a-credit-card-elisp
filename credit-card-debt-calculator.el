
(defun ccdc--raw-months-to-pay-off (balance apr-fraction monthly-payment)
  (let ((inc-i (+ (/ apr-fraction 365.0) 1.0))
        (b-on-p (/ balance monthly-payment)))
    (let ((log-term (+ 1.0 (* b-on-p (- 1 (expt inc-i 30.0))))))
      (if (> log-term 0)
          (- (/ (log log-term)
                (* 30.0 (log inc-i))))
        (error "Unsolvable conditions")))))

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

(defun ccdc--read-positive-value (message)
  (let ((result (read-number message)))
    (if (> result 0)
        result
      (ccdc--read-positive-value message))))

(defun ccdc--read-echo-positive-value (message)
  (insert message)
  (let ((result (ccdc--read-positive-value (format "%s " message))))
    (insert (format " %g\n" result))
    result))

(defun ccdc--red-text (text)
  (put-text-property 1 (length text) 'font-lock-face '(:foreground "red") text)
  text)

(defun ccdc--procedure ()
  (condition-case var
   (insert (format "\nIt will take you %d months to pay off this card."
                   (credit-card-compute-months-to-pay-off (ccdc--read-echo-positive-value "What is your balance?")
                                                          (ccdc--read-echo-positive-value "What is the APR of the card (as percent)?")
                                                          (ccdc--read-echo-positive-value "What is the monthly payment you can make?"))))
   (error (insert (ccdc--red-text "\nThe payment is too low: you will never pay the debit off.")))))

(defun ccdc--mode ()
  (kill-all-local-variables)  
  (setq major-mode 'credit-card-calculator-mode)
  (setq mode-name ccdc--mode-name)
  (run-hooks 'credit-card-mode-hook))

(defun credit-card-compute-months ()
  (interactive)
  (ccdc--switch-to-new-buffer)
  (ccdc--mode)
  (ccdc--procedure))

(provide 'credit-card-debt-calculator)
