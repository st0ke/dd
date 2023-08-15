(in-package #:mapgen)

; TODO enemies? lights?
(defstruct (labyrinth-2d
             (:constructor make-labyrinth-2d (&key size-x size-y)))
  (h-walls (make-array (list size-x (- size-y 1))))
  (v-walls (make-array (list (- size-x 1) size-y))))

(defun labyrinth-2d-size (l2d)
  (let ((h-walls (labyrinth-2d-h-walls l2d)) (v-walls (labyrinth-2d-v-walls l2d)))
    (list (cons :x (array-dimension h-walls 0)) (cons :y (array-dimension v-walls 1)))))

(defmacro labyrinth-2d-with-sizes ((x y) l2d &body body)
  (let ((size (gensym "SIZE")))
    `(let ((,size (labyrinth-2d-size l2d)))
       (let ((,x (cdr (assoc :x ,size))) (,y (cdr (assoc :y ,size))))
         ,@body))))

(defun labyrinth-2d-cell (l2d x y)
  (let ((h-walls (labyrinth-2d-h-walls l2d)) (v-walls (labyrinth-2d-v-walls l2d)))
    (list (cons :top
                (if (> y 0)
                  (aref h-walls x (- y 1))
                  T))
          (cons :bottom
                (if (< y (- (array-dimension h-walls 1) 1))
                  (aref h-walls x y)
                  T))
          (cons :left
                (if (> x 0)
                  (aref v-walls (- x 1) y)
                  T))
          (cons :right
                (if (< x (- (array-dimension v-walls 0) 1))
                  (aref v-walls x y)
                  T)))))

(defstruct (labyrinth-2d-generation-config)
  room
  cell-size
  cell-counts)

(defun labyrinth-2d-generation-config-size (config)
  (3d-vectors:vec3
   (* (labyrinth-2d-generation-config-cell-size config) (cdr (assoc :x (labyrinth-2d-generation-config-cell-counts config))))
   (* (labyrinth-2d-generation-config-cell-size config) (cdr (assoc :y (labyrinth-2d-generation-config-cell-counts config))))
   (room-generation-config-height (labyrinth-2d-generation-config-room config))))

(defun map-content-by-labyrinth-2d (l2d config)
  (let ((h-walls (labyrinth-2d-h-walls l2d))
        (v-walls (labyrinth-2d-v-walls l2d))
        (size (labyrinth-2d-size l2d))
        (height (room-generation-config-height (labyrinth-2d-generation-config-room config)))
        (wall-texture (room-generation-config-wall-texture (labyrinth-2d-generation-config-room config)))
        (cell-size (labyrinth-2d-generation-config-cell-size config))
        (offset (3d-vectors:vec3 
                 (* -0.5 (* (labyrinth-2d-generation-config-cell-size config) (cdr (assoc :x (labyrinth-2d-generation-config-cell-counts config)))))
                 (* -0.5 (* (labyrinth-2d-generation-config-cell-size config) (cdr (assoc :y (labyrinth-2d-generation-config-cell-counts config)))))
                 0)))
    (make-map-content
      :brushes
      (nconc
        (let ((brushes ()))
          (dotimes (y (cdr (assoc :y size)) brushes)
            (dotimes (x (cdr (assoc :x size)))
              (progn
                (when (< y (- (array-dimension h-walls 1) 1))
                  (when (aref h-walls x y)
                    (push
                      (make-brush-box
                        (3d-vectors:v+
                         (3d-vectors:vec3 (+ (* x cell-size) (* cell-size 0.5)) (* (+ y 1) cell-size) (* height 0.5))
                         offset)
                        (3d-vectors:vec3 cell-size 2 height)
                        wall-texture)
                      brushes)))
                (when (< x (- (array-dimension v-walls 0) 1))
                  (when (aref v-walls x y)
                    (push
                      (make-brush-box
                        (3d-vectors:v+
                         (3d-vectors:vec3 (* (+ x 1) cell-size) (+ (* y cell-size) (* cell-size 0.5)) (* height 0.5))
                         offset)
                        (3d-vectors:vec3 2 cell-size height)
                        wall-texture)
                      brushes)))
                ))))))))

(defun labyrinth-2d-generate (l2d config)
  (combine-map-content
    (generate-world-box
      (labyrinth-2d-generation-config-size config)
      (labyrinth-2d-generation-config-room config))
    (map-content-by-labyrinth-2d l2d config)
    (make-map-content ; TODO
      :entities (nconc
                  (list (make-player-start-entity :name "start_1" :origin "64 0 0"))
                  (labyrinth-2d-with-sizes
                    (size-x size-y) l2d
                    (loop for x to (- size-x 1)
                          nconc (loop for y to (- size-y 1)
                                      collect (make-point-light-entity
                                                ; TODO
                                                :name (format NIL "light_~A~A" x y)
                                                :origin (format NIL "~F ~F 96"
                                                                (+ (* (- x (/ size-x 2)) 128) 64)
                                                                (+ (* (- y (/ size-y 2)) 128) 64))
                                                :light-radius "128 128 128"
                                                :color (format NIL "~F ~F ~F"
                                                               (+ 0.1 (random 0.9))
                                                               (+ 0.1 (random 0.9))
                                                               (+ 0.1 (random 0.9)))))))))))

(defun labyrinth-2d-manual-test ()
  (save-map
    "testlab.map"
    ;(let ((l2d (make-labyrinth-2d :size-x 32 :size-y 32)))
    (let ((l2d (make-labyrinth-2d-square-by-array
                 (first
                   (search-algo
                     :generator #'(lambda () (search-algo-array-generator (* (* 8 7) 2)))
                     :mutator #'(lambda (orig gen) (search-algo-array-mutator 0.1 1.0 orig))
                     :score-counter #'(lambda (data)
                                        (labyrinth-2d-count-score-0 (make-labyrinth-2d-square-by-array data 8)))
                     :n-elite 32
                     :reproduction-factor 2
                     :n-generations 20))
                 8)))
      (progn
        (labyrinth-2d-generate l2d
                               (make-labyrinth-2d-generation-config
                                 :room *default-room-config*
                                 :cell-size 128
                                 :cell-counts '((:x . 8) (:y . 8))))))))

;(let ((size (/ (array-dimension data 0) 2)))
(defun make-labyrinth-2d-square-by-array (data size)
  (let ((l2d (make-labyrinth-2d :size-x size :size-y size)))
    (let ((h-walls (labyrinth-2d-h-walls l2d))
          (v-walls (labyrinth-2d-v-walls l2d)))
        (let ((small-size (- size 1)))
          (dotimes (i (* size small-size) l2d)
            (progn
              (setf (aref h-walls (truncate (/ i small-size)) (mod i small-size))
                    (< (aref data i) 0.5))
              (setf (aref v-walls (mod i small-size) (truncate (/ i small-size)))
                    (< (aref data i) 0.5))))))))

(defmacro labyrinth-2d-has-path-process-neighbour (queue marks cell direction-name x y)
  `(when (and
           (not (cdr (assoc ,direction-name ,cell)))
           (not (aref ,marks ,x ,y)))
     (progn
       (push (list ,x ,y) ,queue)
       (setf (aref ,marks ,x ,y) 1))))

(defmacro labyrinth-2d-with-marks ((marks has-path) l2d from-x from-y to-x to-y &body body)
  (let ((size-x (gensym "SIZE-X")) (size-y (gensym "SIZE-Y")))
    `(labyrinth-2d-with-sizes
       (,size-x ,size-y) ,l2d
       (let ((,marks (make-array (list ,size-x ,size-y)))
             (,has-path NIL))
         (progn
           (setf (aref ,marks ,from-x ,from-y) 1)
           (let ((queue (list (list ,from-x ,from-y))))
             (loop named queue-processing
                   while queue
                   do (let ((current (pop queue)))
                        (let ((x (first current)) (y (second current)))
                          (progn
                            (when (and (= ,to-x x) (= ,to-y y))
                              (setf ,has-path T))
                            (let ((cell (labyrinth-2d-cell ,l2d x y)))
                              (progn
                                (labyrinth-2d-has-path-process-neighbour queue ,marks cell :top x (- y 1))
                                (labyrinth-2d-has-path-process-neighbour queue ,marks cell :bottom x (+ y 1))
                                (labyrinth-2d-has-path-process-neighbour queue ,marks cell :left (- x 1) y)
                                (labyrinth-2d-has-path-process-neighbour queue ,marks cell :right (+ x 1) y))))))))
           ,@body)))))

(defun labyrinth-2d-count-unreachable-cells (marks)
  (let ((out 0))
    (dotimes (x (array-dimension marks 0) out)
      (dotimes (y (array-dimension marks 1))
        (when (not (aref marks x y)) (incf out))))))

(defun labyrinth-2d-count-score-0 (l2d)
  (labyrinth-2d-with-sizes
    (size-x size-y) l2d
    (labyrinth-2d-with-marks
      (marks has-path) l2d 0 0 (- size-x 1) (- size-y 1)
      (+ (if has-path 0 -10000)
         (* -2 (labyrinth-2d-count-unreachable-cells marks))
         ))))
;(labyrinth-2d-count-room-cells l2d)))))


