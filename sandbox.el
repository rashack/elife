;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sandbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new-map (list
          (cons 1 2)
          (cons 5 6)))




((lambda (xx yy)

  (loop for x from (- xx 1) to (+ xx 1) collect x)
  (loop for y from (- yy 1) to (+ yy 1) collect x)

) 3 5)
