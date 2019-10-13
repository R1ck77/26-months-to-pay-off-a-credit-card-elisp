
(defun generate-supplier (results)
  (lexical-let ((results results))
    (lambda (&optional ignored)
      (let ((next-result (first results)))
        (setq results (rest results))
        next-result))))

(provide 'test-utils)
