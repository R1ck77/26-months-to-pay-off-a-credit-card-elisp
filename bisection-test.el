(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'bisection)

(describe "bisection.el"
  (describe "bisect"
    (it "fails if the function doesn't change sign in the interval provided"
      (should-error (bisect 'identity -100 -10))
      (should-error (bisect 'identity 10 100)))))
