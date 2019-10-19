
;;; TODO/FIXME THIS SUCKS BIG TIME. Use a result list to support
(defun bisect--valid-argumentsp (f neg-x pos-x)
  (let ((neg-v (funcall f neg-x))
        (pos-v (funcall f pos-x)))
    (when (> neg-v pos-v)
      (let ((tmp neg-v))
        (setq neg-v pos-v)
        (setq pos-v tmp)))
    (and (<= neg-v 0)
         (>= pos-v  0))))

(defun bisect (f neg-x pos-x)
  (if (not (bisect--valid-argumentsp f neg-x pos-x))
      (error "Invalid interval: the function doesn't change sign"))
  )

(provide 'bisection)
