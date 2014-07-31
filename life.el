
(defun alive (world cell)
  (not (null (gethash cell world))))

(defun alive2 (world x y)
  (alive world (cons x y)))

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

(defun new-cell (x y)
  '(pos (cons x y) age 0))

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

(defun assertEqual (exp val)
  (equal exp val))

(defun assertEqualM (expected act)
  (let ((actual (unwind-protect
                    (condition-case err
                        (act)
                      ('error (message (format "Caught exception: [%s]" ex)))))))
                      ;;('error (concat "%s" (error-message-string err)))))))
    (if (equal actual expected)
        t
      (concat "Expected: " expected " actual was: " actual " for expression " act))))

  (assertEqualM 0 (neighbours (new-world '())
                              (new-cell 1 1)))



;; test neighbours
(progn
  (assertEqual 0 (neighbours (new-world '())
                             (new-cell 1 1)))
  (assertEqual 1 (neighbours (new-world (list (cons 0 1)))
                             (new-cell 1 1)))
  (assertEqual 2 (neighbours (new-world (list (new-cell 1 1)
                                              (new-cell 2 1)
                                              (new-cell 3 1)))
                             (new-cell 2 1)))
  (assertEqual 1 (neighbours (new-world (list (new-cell 1 1)
                                              (new-cell 2 1)
                                              (new-cell 3 1)))
                             (new-cell 1 1)))
  )

;; test alive
(progn
  (assertEqual nil
               (alive (new-world '())
                      (cons 0 0)))
  (assertEqual t
               (alive (new-world (list (cons 0 0)))
                      (cons 0 0)))
  (assertEqual nil
               (alive (new-world (list (cons 0 0)))
                      (cons 1 1)))
  )

;; test survives
(progn
  (assertEqual t
               (survives (new-world (list (new-cell 1 1)
                                          (new-cell 2 1)
                                          (new-cell 3 1)))
                         (new-cell 2 1)))
  (assertEqual nil
               (survives (new-world (list (new-cell 1 1)
                                          (new-cell 2 1)))
                         (new-cell 2 1)))
  )
