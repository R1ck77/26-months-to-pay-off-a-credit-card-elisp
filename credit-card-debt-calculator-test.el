(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'credit-card-debt-calculator)

(describe "credit-card-debt-calculator.el"
  (describe "credit-card-compute-months-to-pay-off"
    (it "computes the months to pay off your credit card correctly"
      (expect (credit-card-compute-months-to-pay-off 5000 12 100)
              :to-be 70)))
  (describe "credit-card-compute-payments-to-pay-off"
    (it "returns an inverse of the credit-card-compute-months-to-pay-off"
      (let ((result (credit-card-compute-payments-to-pay-off 5000 13 14)))
        (expect (credit-card-compute-months-to-pay-off 5000 13 result)
                :to-be 14))))
  (describe "credit-card-calculation"
    (before-each
      (kill-buffer (get-buffer ccdc--buffer-name)))
    (it "creates a new buffer with the correct name and mode, even if another was already present"
      (with-current-buffer (get-buffer-create "* Credit Card Calculator Mode *")
        (insert "Text I don't want to see")
        (setq buffer-read-only t))
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 -12 100)))
      (spy-on 'read-string :and-return-value "t")      
      (credit-card-calculation)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "Do you want to compute the time to pay the debt off (t) or the money required ($)? t
What is your balance? 5000.00
What is the APR of the card (as percent)? 12.00
What is the monthly payment you can make? 100.00

It will take you 70 months to pay off this card."))
    (it "handles the case where the monthly payment is too low to pay off the debt"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 20)))
      (spy-on 'read-string :and-return-value "t")
      (credit-card-calculation)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "Do you want to compute the time to pay the debt off (t) or the money required ($)? t
What is your balance? 5000.00
What is the APR of the card (as percent)? 12.00
What is the monthly payment you can make? 20.00

The payment is too low: you will never pay the debit off."))
    (it "prints the warning for unsolvability in red"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 20)))
      (spy-on 'read-string :and-return-value "t")
      (credit-card-calculation)
      (goto-char (point-max))
      (search-backward "The payment is too low")
      (expect (get-text-property (point) 'font-lock-face )
              :to-equal '(:foreground "red")))
    (it "solves the inverse problem"
      (let* ((balance 5000)
            (apr 12)
            (months-to-pay-off 70)
            (expected-result (credit-card-compute-payments-to-pay-off balance apr months-to-pay-off)))
        (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 70)))
        (spy-on 'read-string :and-return-value "$")
        (credit-card-calculation)
        (expect (buffer-substring (point-min) (point-max))
                :to-equal (format "Do you want to compute the time to pay the debt off (t) or the money required ($)? $
What is your balance? 5000.00
What is the APR of the card (as percent)? 12.00
How long do you want to pay (months)? 70

It will take %g$ payments to pay off the debt in the period selected." (round-to-upper-cent expected-result)))))
    (it "prints an appropriate error if the starting conditions are out of bounds"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 1000000 1)))
      (spy-on 'read-string :and-return-value "$")
      (credit-card-calculation)
      (expect (buffer-substring (point-min) (point-max))
                :to-equal (format "Do you want to compute the time to pay the debt off (t) or the money required ($)? $
What is your balance? 5000.00
What is the APR of the card (as percent)? 1000000.00
How long do you want to pay (months)? 1

Invalid starting conditions.")))))



