(setq load-path (cons "." load-path))
(require 'bisection)

(defvar credit-card-calculator-mode nil
  "Mode variable for \"Credit Card Calculator Mode\"")

(defvar credit-card-mode-hook nil
  "Hook functions for \"Credit Card Calculator Mode\"")

(defconst ccdc--mode-name "Credit Card Calculator Mode")
(defconst ccdc--buffer-name (concat "* " ccdc--mode-name " *"))

(defconst ccdc--computation-mode-query "Do you want to compute the time to pay the debt off (t) or the money required ($)? ")

(defun ccdc--raw-months-to-pay-off (balance apr-fraction monthly-payment)
  (let ((inc-i (+ (/ apr-fraction 365.0) 1.0))
        (b-on-p (/ balance (float monthly-payment))))
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

(defun ccdc--generate-inverse-function (balance apr months)
  (lexical-let ((balance balance)
                (apr apr)
                (months months))
    (lambda (x)
      (condition-case nil
          (- (credit-card-compute-months-to-pay-off balance apr x) months)
          (error 1)))))

(defun credit-card-compute-payments-to-pay-off (balance apr months)
  (bisect-cached (ccdc--generate-inverse-function balance apr months)
                 0
                 (* 30 balance)
                 0.001))

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

(defun ccdc--read-echo-positive-value (message &optional format-string)
  (insert message)
  (let ((result (ccdc--read-positive-value (format "%s " message)))
        (format-string (or format-string "%s")))
    (insert (format (concat " " format-string "\n") result))
    result))

(defun ccdc--red-text (text)
  (put-text-property 1 (length text) 'font-lock-face '(:foreground "red") text)
  text)

(defun ccdc--read-computation-mode ()
  (let ((result (read-string ccdc--computation-mode-query)))
    (cond
     ((equal result "$") "$")
     ((equal (downcase result) "t") "t")
     (:otherwise (ccdc--read-computation-mode)))))

(defun ccdc--ask-calculation-mode ()
  (insert ccdc--computation-mode-query)
  (let ((result (ccdc--read-computation-mode)))
    (insert result)
    (insert "\n")
    (cond
      ((equal result "$") :money-to-pay-debt)
      ((equal result "t") :time-to-pay-debt))))

(defun ccdc--read-balance ()
  (ccdc--read-echo-positive-value "What is your balance?" "%.2f"))

(defun ccdc--read-apr ()
  (ccdc--read-echo-positive-value "What is the APR of the card (as percent)?" "%.2f"))

(defun ccdc--time-to-pay-debt-procedure ()
  (condition-case var
   (insert (format "\nIt will take you %d months to pay off this card."
                   (credit-card-compute-months-to-pay-off (ccdc--read-balance)
                                                          (ccdc--read-apr)
                                                          (ccdc--read-echo-positive-value "What is the monthly payment you can make?" "%.2f"))))
   (error (insert (ccdc--red-text "\nThe payment is too low: you will never pay the debit off.")))))

(defun round-to-upper-cent (value)
  (/ (ceiling (* value 100)) 100.0))

(defun ccdc--money-to-pay-debt-procedure ()
  (condition-case nil
      (insert (format "\nIt will take %g$ payments to pay off the debt in the period selected."
                      (round-to-upper-cent
                       (credit-card-compute-payments-to-pay-off (ccdc--read-balance)
                                                                (ccdc--read-apr)
                                                                (ccdc--read-echo-positive-value "How long do you want to pay (months)?")))))
    (error (insert (ccdc--red-text "\nInvalid starting conditions.")))))

(defun ccdc--procedure ()
  (case (ccdc--ask-calculation-mode)
    (:money-to-pay-debt (ccdc--money-to-pay-debt-procedure))
    (:time-to-pay-debt (ccdc--time-to-pay-debt-procedure))
    (t (error "Unexpected result for the calculation mode!"))))

(defun ccdc--mode ()
  (kill-all-local-variables)  
  (setq major-mode 'credit-card-calculator-mode)
  (setq mode-name ccdc--mode-name)
  (run-hooks 'credit-card-mode-hook))

(defun credit-card-calculation  ()
  (interactive)
  (ccdc--switch-to-new-buffer)
  (ccdc--mode)
  (ccdc--procedure))

(provide 'credit-card-debt-calculator)
