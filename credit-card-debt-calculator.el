
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

(defun credit-card-calculator-mode ())

(provide 'credit-card-debt-calculator)
