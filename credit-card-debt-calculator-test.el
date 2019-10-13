(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'credit-card-debt-calculator)

(describe "credit-card-debt-calculator.el"
  (describe "credit-card-compute-months-to-pay-off"
    (it "computes the months to pay off your credit card correctly"
      (expect (credit-card-compute-months-to-pay-off 5000 12 100)
              :to-be 70)))
  (describe "credit-card-compute-months"
    (before-each
      (kill-buffer (get-buffer ccdc--buffer-name)))
    (it "creates a new buffer with the correct name and mode, even if another was already present"
      (with-current-buffer (get-buffer-create "* Credit Card Calculator Mode *")
        (insert "Text I don't want to see")
        (setq buffer-read-only t))
      (spy-on 'read-number :and-call-fake (generate-supplier (list 5000 12 -12 100)))
      (credit-card-compute-months)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "What is your balance? 5000
What is the APR of the card (as percent)? 12
What is the monthly payment you can make? 100

It will take you 70 months to pay off this card."))))


