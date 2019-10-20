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
        (expect (<= (abs (- result -5)) 0.0003)))))
  (describe "memoize-function"
    (it "returns the result of the memoized function"
      (spy-on 'identity :and-call-through)
      (let ((memoized (memoize-function 'identity)))
        (expect (funcall memoized 12) :to-be 12)
        (expect (funcall memoized "12") :to-equal "12")
        (expect (funcall memoized 7) :to-be 7)
        (expect (funcall memoized nil) :to-be nil)))
    (it "invokes the memoized function only once per value"
      (spy-on 'identity :and-call-through)
      (let ((memoized (memoize-function 'identity)))
        (expect (funcall memoized 12) :to-be 12)
        (expect (funcall memoized 42) :to-be 42)
        (expect (funcall memoized 12) :to-be 12)
        (expect (funcall memoized 42) :to-be 42)
        (expect (funcall memoized 12) :to-be 12)
        (expect 'identity :to-have-been-called-times 2)))
    (it "invokes the memoized function once even if the result is nil"
      (spy-on 'identity :and-call-through)
      (let ((memoized (memoize-function 'identity)))
        (expect (funcall memoized nil) :to-be nil)
        (expect (funcall memoized nil) :to-be nil)
        (expect (funcall memoized nil) :to-be nil)
        (expect 'identity :to-have-been-called-times 1))))
  (xdescribe "bisect-cached"
    (it "invokes the function only twice if both extremes are negative"
      (spy-on 'identity)
      (should-error (bisect-cached 'identity -100 -10))
      (expect 'identity :to-have-been-called-times 2))
    (it "invokes the function only twice if both extremes are positive"
      (spy-on 'identity)
      (should-error (bisect-cached 'identity 100 10))
      (expect 'identity :to-have-been-called-times 2))))
