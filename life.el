
(defun alive (world cell)
  (not (null (gethash cell world))))

;; return number of alive neighbours
(defun neighbours (world cell)
  (reduce '+ (mapcar (lambda (c) (if c 1 0))
                     (mapcar (lambda (c) (alive world c))
                             (neighbour-cells cell)))))

(defun neighbour-cells (cell)
  (let ((x (cell-x cell))
        (y (cell-y cell)))
    (list (cell-new (1- x) (1- y))
          (cell-new     x  (1- y))
          (cell-new (1+ x) (1- y))
          (cell-new (1- x)     y)
          (cell-new (1+ x)     y)
          (cell-new (1- x) (1+ y))
          (cell-new     x  (1+ y))
          (cell-new (1+ x) (1+ y)))))

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

(defun world-new (cells)
  (let ((world (make-hash-table :test 'equal)))
    (mapcar (lambda (c) (world-add-cell world c)) cells)
    world))

(defun world-add-cell (world cell)
  (puthash cell cell world))

;; does this cell survive to the next cycle?
(defun survives (world cell)
  (let ((n-neighbours (neighbours world cell)))
    (and (<= n-neighbours 3) (>= n-neighbours 2))))

;; will this dead cell come to life?
(defun spawns (world cell)
  (let ((n-neighbours (neighbours world cell)))
    (= n-neighbours 3)))

(defun cycle (world)
  (let ((next-world (world-new '()))
        (checked-cells (make-hash-table :test 'equal)))
    (maphash (lambda (cell _alive)
               (when (survives world cell)
                 (world-add-cell next-world cell))
               (puthash cell cell checked-cells)
               (mapcar (lambda (nc)
                         (when (null (gethash nc checked-cells))
                           (puthash nc nc checked-cells)
                           (when (spawns world nc)
                             (world-add-cell next-world nc))))
                       (neighbour-cells cell)))
             world)
    next-world))

(defun insert-xy (x y char)
  (beginning-of-buffer)
  (let ((next-line-add-newlines t))
    (dotimes (l y)
      (next-line)))
  (dotimes (c x)
    (if (equal (point) (point-at-eol))
        (insert " ")
      (right-char)))
  (insert-char char)
  (when (not (point-at-eol))
    (delete-char 1)))

(defun insert-world (world)
  (maphash (lambda (_key cell)
             (insert-xy (cell-x cell) (cell-y cell) ?¤))
           world))

(defun animate-life (world frame-delay cycles)
  (let ((buffer "life-buffer"))
    (with-current-buffer (get-buffer-create buffer)
      (switch-to-buffer buffer)
      (overwrite-mode)
      (dotimes (x cycles)
        (erase-buffer)
        (insert-world world)
        (beginning-of-buffer)
        (sit-for frame-delay)
        (setq world (cycle world))
      ))))

(defun glider ()
  (world-new (list (cell-new 2 1)
                   (cell-new 3 2)
                   (cell-new 1 3) (cell-new 2 3) (cell-new 3 3))))

(defun blinker ()
  (world-new (list (cell-new 4 2)
                   (cell-new 4 3)
                   (cell-new 4 4))))

;; play

(animate-life (glider) 0.1 100)
(animate-life (blinker) 0.2 1)
(animate-life (cycle (blinker)) 0.2 1)

(animate-life
 (world-new (list (cell-new 6 4)
                  (cell-new 7 4)))
 0.2 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assertEqual (expected expr)
  `(let ((actual ,expr))
     (if (equal actual ,expected)
         t
       (message "Expected: '%s' Actual: '%s' Expression: '%s'"
                ,expected actual (prin1-to-string ',expr)))))

(defun test-many (funs)
  (let* ((res (mapcar 'eval funs))
         (failed (filter (lambda (r) (not (equal t r)))
                         res)))
    (if (null failed)
        (format "All %d tests succeeded" (length res))
      (format "%d of %d tests failed: %s" (length failed) (length res) failed))))

;; test neighbours
(test-many
 '((assertEqual 0 (neighbours (world-new '())
                              (cell-new 1 1)))
   (assertEqual 1 (neighbours (world-new (list (cell-new 0 1)))
                              (cell-new 1 1)))
   (assertEqual 2 (neighbours (world-new (list (cell-new 1 1)
                                               (cell-new 2 1)
                                               (cell-new 3 1)))
                              (cell-new 2 1)))
   (assertEqual 1 (neighbours (world-new (list (cell-new 1 1)
                                               (cell-new 2 1)
                                               (cell-new 3 1)))
                              (cell-new 1 1)))
   (assertEqual 4 (neighbours (world-new (list (cell-new 1 1)
                                               (cell-new 3 1)
                                               (cell-new 1 3)
                                               (cell-new 3 3)))
                              (cell-new 2 2)))
   (assertEqual 6 (neighbours (world-new (list (cell-new 1 1)
                                               (cell-new 2 1)
                                               (cell-new 3 1)
                                               (cell-new 1 3)
                                               (cell-new 2 3)
                                               (cell-new 3 3)))
                              (cell-new 2 2)))))

;; test alive
(test-many
 '((assertEqual nil
                (alive (world-new '())
                       (cell-new 0 0)))
   (assertEqual t
                (alive (world-new (list (cell-new 0 0)))
                       (cell-new 0 0)))
   (assertEqual nil
                (alive (world-new (list (cell-new 0 0)))
                       (cell-new 1 1)))))

;; test survives
(test-many
 '((assertEqual t
                (survives (world-new (list (cell-new 1 1)
                                           (cell-new 2 1)
                                           (cell-new 3 1)))
                          (cell-new 2 1)))
   (assertEqual nil
                (survives (world-new (list (cell-new 1 1)
                                           (cell-new 2 1)))
                          (cell-new 2 1)))))

;; test cycle
(let ((empty-from-one (cycle (world-new (list (cell-new 1 1)))))
      (empty-from-two (cycle (world-new (list (cell-new 1 1)
                                              (cell-new 1 2))))))
  (test-many
   '(;; blinker
     (assertEqual t (alive (cycle (blinker)) (cell-new 3 3)))
     (assertEqual t (alive (cycle (blinker)) (cell-new 4 3)))
     (assertEqual t (alive (cycle (blinker)) (cell-new 5 3)))
     (assertEqual 3 (hash-table-count (cycle (blinker))))
     ;; one cell to empty
     (assertEqual nil (alive empty-from-one (cell-new 1 1)))
     (assertEqual 0 (hash-table-count empty-from-one))
     ;; two cells to empty
     (assertEqual nil (alive empty-from-two (cell-new 1 1)))
     (assertEqual 0 (hash-table-count empty-from-two))
     )))


;; unused testing stuff

(defun flatten (lst)
  "Flatten a list of lists with pairs, leave the pairs intact."
  (cond ((null lst) '())
        ((atom lst) (list lst))
        ;; if it is a pair, don't try to flatten it
        ((and (listp (car lst))
              (not (listp (cdr (car lst)))))
         (cons (car lst) (flatten (cdr lst))))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))

(defun neighbour-cells (cx cy)
  (flatten
   (loop for x in (number-sequence (1- cx) (1+ cx))
         collect (loop for y in (number-sequence (1- cy) (1+ cy))
                       when (not (and (equal cx x)
                                      (equal cy y)))
                       collect (cons x y)))))

(neighbour-cells 5 9)

(defun neighbour-cells (x y)
  (list (cons (1- x) (1- y))
        (cons     x  (1- y))
        (cons (1+ x) (1- y))
        (cons (1- x)     y)
        (cons (1+ x)     y)
        (cons (1- x) (1+ y))
        (cons     x  (1+ y))
        (cons (1+ x) (1+ y))))
