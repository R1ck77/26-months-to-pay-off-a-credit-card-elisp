(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'bisection)

(describe "bisection.el"
  (describe "bisect"
    (it "fails if the function doesn't change sign in the interval provided"
      (should-error (bisect 'identity -100 -10))
      (should-error (bisect 'identity 10 100)))
    (it "returns one of the starting points if it's a node within the error"
      (expect (bisect 'identity 0.1 100 0.1)
              :to-equal 0.1)
      (expect (bisect 'identity 10 0.009 0.01)
              :to-equal 0.009)
      (expect (bisect 'identity 10 0.01)
              :to-equal 0.01)
      (expect (bisect (lambda (x) (+ 1 x))
                      -1 0)
              :to-equal -1))
    (it "returns the bisection of the solution with the default error"
      (let ((result (bisect (lambda (x) (+ x 5))
                            -11.2 100)))
        (expect (<= (abs (- result -5)) 0.01))))
    (it "returns the bisection of the solution with a custom error"
      (let ((result (bisect (lambda (x) (+ x 5))
                            -11.2 100 0.0003)))
        (expect (<= (abs (- result -5)) 0.0003))))))
