(require 'cl)

;;; TODO/FIXME add a cache of evaluated points  

(defconst default-bisection-error 0.01
  "we assume that a bisection without explicit error wants less than this absolute error")

(defun bisect--sort-arguments (f &rest positions)
  (let ((evaluated (mapcar f positions)))
    (if (< (first evaluated) (second evaluated))
        positions
      (reverse positions))))

(defun bisect--valid-argumentsp (f pos)
  (let ((evaluated (mapcar f pos)))
    (and (<= (first evaluated) 0)
         (>= (second evaluated) 0))))

(defun bisect--within-errorp (value error)
  (<= (abs value) error))

(defun bisect--solutionp (f pos error)
  (let ((evaluated (mapcar f pos)))
    (if (bisect--within-errorp (first evaluated) error)
        (first pos)
      (if (bisect--within-errorp (second evaluated) error)
          (second pos)))))

(defun bisect--next-interval (f pos)
  (let ((evaluated (mapcar f pos))
        (middle-point (/ (apply '+ pos) 2)))
    (if (> (funcall f middle-point) 0)
        (list (first pos) middle-point)
      (list middle-point (second pos)))))

(defun bisect--recursively (f pos error)
  (let ((solution (bisect--solutionp f pos error)))
    (or solution
        (bisect--recursively f (bisect--next-interval f pos) error))))

(defun bisect (f neg-x pos-x &optional error)
  (let ((error (or error default-bisection-error)))
    (let ((solution (bisect--solutionp f (list neg-x pos-x) error)))
      (or solution
          (let ((sorted-pos (bisect--sort-arguments f neg-x pos-x)))
            (if (not (bisect--valid-argumentsp f sorted-pos))
                (error "Invalid interval: the function doesn't change sign")
              (bisect--recursively f sorted-pos error)))))))

(defun memoize-function (f)
  "Create a memoized version of the function (one argument only)"
  (lexical-let ((f f)
                (hash-table (make-hash-table)))
    (lambda (x)
      (let* ((default (make-symbol "default"))
             (cached (gethash x hash-table default)))
        (if (eq cached default)
            (let ((result (funcall f x)))
              (puthash x result hash-table)
              result)
            cached)))))

(defun bisect-cached (f neg-x pos-x &optional error)
  (let ((cached-f (memoize-function f)))
    (bisect cached-f neg-x pos-x error)))

(provide 'bisection)
