
(defun alive (world cell)
  (not (null (gethash cell world))))

(defun alive2 (world x y)
  (alive world (cell-new x y)))

;; return number of alive neighbours
(defun neighbours (world cell)
  (let ((x (cell-x cell))
        (y (cell-y cell)))
    (reduce '+ (mapcar (lambda (c) (if c 1 0))
                       (list
                        (alive2 world (- x 1) (- y 1))
                        (alive2 world    x    (- y 1))
                        (alive2 world (+ x 1) (- y 1))
                        (alive2 world (- x 1)    y)
                        (alive2 world (+ x 1)    y)
                        (alive2 world (- x 1) (+ y 1))
                        (alive2 world    x    (+ y 1))
                        (alive2 world (+ x 1) (+ y 1)))))))

(defun cell-pos (cell)
  (plist-get cell 'pos))

(defun cell-x (cell)
  (car (cell-pos cell)))

(defun cell-y (cell)
  (cdr (cell-pos cell)))

(defun cell-age (cell)
  (plist-get cell 'age))

(defun cell-weight (cell)
  (let ((w (plist-get cell 'weight)))
    (if w w (random))))

(defun cell-new (x y)
  `(pos ,(cons x y) age 0))

(defun new-world (cells)
  (let ((world (make-hash-table :test 'equal)))
    (mapcar (lambda (c) (puthash c c world)) cells)
    world))

;; does this cell survive to the next cycle?
(defun survives (world cell)
  (let ((n-neighbours (neighbours world cell)))
    (if (and (<= n-neighbours 3) (>= n-neighbours 2))
        t
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assertEqualMC (expected expr)
  `(let ((actual (condition-case ex
                     ,expr
                   ('error (message (format "Caught exception: [%s]" ex))))))
     (if (equal actual ,expected)
         t
       (message "Expected: '%s' Actual: '%s' Expression: '%s'"
                ,expected actual (prin1-to-string ',expr)))))

(defmacro assertEqual (expected expr)
  `(let ((actual ,expr))
     (if (equal actual ,expected)
         t
       (message "Expected: '%s' Actual: '%s' Expression: '%s'"
                ,expected actual (prin1-to-string ',expr)))))

(macroexpand
 ' (assertEqual 0
                (neighbours (new-world '())
                            (cell-new 1 1)))
 )

(assertEqual 1
             (/ 1 1))

;; test neighbours
(progn
  (assertEqual 0 (neighbours (new-world '())
                             (cell-new 1 1)))
  (assertEqual 1 (neighbours (new-world (list (cell-new 0 1)))
                             (cell-new 1 1)))
  (assertEqual 2 (neighbours (new-world (list (cell-new 1 1)
                                              (cell-new 2 1)
                                              (cell-new 3 1)))
                             (cell-new 2 1)))
  (assertEqual 1 (neighbours (new-world (list (cell-new 1 1)
                                              (cell-new 2 1)
                                              (cell-new 3 1)))
                             (cell-new 1 1)))
  (assertEqual 4 (neighbours (new-world (list (cell-new 1 1)
                                              (cell-new 3 1)
                                              (cell-new 1 3)
                                              (cell-new 3 3)))
                             (cell-new 2 2)))
  (assertEqual 6 (neighbours (new-world (list (cell-new 1 1)
                                              (cell-new 2 1)
                                              (cell-new 3 1)
                                              (cell-new 1 3)
                                              (cell-new 2 3)
                                              (cell-new 3 3)))
                             (cell-new 2 2)))
  )

;; test alive
(progn
  (assertEqual nil
               (alive (new-world '())
                      (cell-new 0 0)))
  (assertEqual t
               (alive (new-world (list (cell-new 0 0)))
                      (cell-new 0 0)))
  (assertEqual nil
               (alive (new-world (list (cell-new 0 0)))
                      (cell-new 1 1)))
  )

;; test survives
(progn
  (assertEqual t
               (survives (new-world (list (cell-new 1 1)
                                          (cell-new 2 1)
                                          (cell-new 3 1)))
                         (cell-new 2 1)))
  (assertEqual nil
               (survives (new-world (list (cell-new 1 1)
                                          (cell-new 2 1)))
                         (cell-new 2 1)))
  )
