(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'credit-card-debt-calculator)

(describe "credit-card-debt-calculator.el"
  (describe "credit-card-compute-months-to-pay-off"
    (it "computes the months to pay off your credit card correctly"
      (expect (credit-card-compute-months-to-pay-off 5000 12 100)
              :to-be 70)))
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
What is your balance? 5000
What is the APR of the card (as percent)? 12
What is the monthly payment you can make? 100

It will take you 70 months to pay off this card."))
    (it "handles the case where the monthly payment is too low to pay off the debt"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 20)))
      (spy-on 'read-string :and-return-value "t")
      (credit-card-calculation)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "Do you want to compute the time to pay the debt off (t) or the money required ($)? t
What is your balance? 5000
What is the APR of the card (as percent)? 12
What is the monthly payment you can make? 20

The payment is too low: you will never pay the debit off."))
    (it "Prints the warning for unsolvability in red"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 20)))
      (spy-on 'read-string :and-return-value "t")
      (credit-card-calculation)
      (goto-char (point-max))
      (search-backward "The payment is too low")
      (expect (get-text-property (point) 'font-lock-face )
              :to-equal '(:foreground "red")))
    (it "Money to solve the debt end-to-end test"
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 70)))
      (spy-on 'read-string :and-return-value "$")
      (credit-card-calculation)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "Do you want to compute the time to pay the debt off (t) or the money required ($)? $
What is your balance? 5000
What is the APR of the card (as percent)? 12
How long do you want to pay (months)? 70

It will take 100$ payments to pay off the debt in the period selected."))))



