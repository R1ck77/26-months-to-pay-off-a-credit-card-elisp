(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'credit-card-debt-calculator)

(describe "credit-card-debt-calculator.el"
  (describe "credit-card-compute-months-to-pay-off"
    (it "computes the months to pay off your credit card correctly"
      (expect (credit-card-compute-months-to-pay-off 5000 12 100)
              :to-be 70))))
