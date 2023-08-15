(in-package #:mapgen)

(defun search-algo (&key
                     generator
                     mutator
                     score-counter
                     n-elite
                     reproduction-factor
                     n-generations)
  (let ((population (loop repeat n-elite collect (funcall generator))))
    (dotimes (gen n-generations population)
      (progn
        (nconc population
               (apply #'nconc (loop for elem in population 
                                    collect (loop repeat reproduction-factor collect (funcall mutator elem gen)))))
        (setf population
              (subseq
                (mapcar #'first
                        (sort 
                          (loop for elem in population collect (list elem (funcall score-counter elem)))
                          #'>
                          :key #'second))
                0 n-elite))))))

(defun search-algo-array-generator (size)
  (let ((out (make-array size)))
    (dotimes (i size out)
      (setf (aref out i) (random 1.0)))))

(defun search-algo-normal-random (std-dev)
  (let ((x (- 1.0 (random 2.0))))
    (* (/ 1 (* std-dev (sqrt (* 2 pi))))
       (exp (* -0.5 (expt (/ x std-dev) 2))))))

(defun search-algo-array-mutator (change-probability std-dev original)
  (let ((size (array-dimension original 0)))
    (let ((out (make-array size)))
      (dotimes (i size out)
        (let ((value (aref original i)))
        (if (< (random 1.0) change-probability)
          (setf (aref out i) (max 0.0 (min 1.0 (+ value (search-algo-normal-random std-dev)))))
          (setf (aref out i) value)))))))
