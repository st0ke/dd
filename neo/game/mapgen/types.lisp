(in-package #:mapgen)

(defstruct texture-mapping
  transform-x ; xxscale xyscale xoffset
  transform-y ; yxscale yyscale yoffset
  filepath)

(defun make-texture-mapping-simple (&key filepath size (scale 1.0))
  (make-texture-mapping
    :transform-x (3d-vectors:vec3 (/ 1 (* (3d-vectors:vx2 size) scale)) 0 0)
    :transform-y (3d-vectors:vec3 0 (/ 1 (* (3d-vectors:vy2 size) scale)) 0)
    :filepath filepath))

(defstruct plane
  normal
  distance)

(defstruct brush-plane
  plane
  texture)

(defstruct brush
  planes) ; brush-planes

(defstruct entity
  classname
  name
  properties)

(defstruct map-content
  brushes
  patches
  entities)

(defun combine-map-content (&rest elems)
  (make-map-content
    :brushes (apply #'nconc (mapcar #'(lambda (mc) (map-content-brushes mc)) elems))
    :patches (apply #'nconc (mapcar #'(lambda (mc) (map-content-patches mc)) elems))
    :entities (apply #'nconc (mapcar #'(lambda (mc) (map-content-entities mc)) elems))))

(defun make-brush-from-planes (planes texture)
  (make-brush :planes (mapcar #'(lambda (plane)
                                  (make-brush-plane :plane plane :texture texture))
                              planes)))

(defun make-brush-box (center size texture)
  (let ((half-size (3d-vectors:v* size 0.5)))
    (make-brush-from-planes
      (list
        (make-plane
          :normal (3d-vectors:vec3 0 0 -1)
          :distance (- (3d-vectors:vz3 center) (3d-vectors:vz3 half-size)))
        (make-plane
          :normal (3d-vectors:vec3 0 0 1)
          :distance (- (+ (3d-vectors:vz3 center) (3d-vectors:vz3 half-size))))
        (make-plane
          :normal (3d-vectors:vec3 0 -1 0)
          :distance (- (3d-vectors:vy3 center) (3d-vectors:vy3 half-size)))
        (make-plane
          :normal (3d-vectors:vec3 0 1 0)
          :distance (- (+ (3d-vectors:vy3 center) (3d-vectors:vy3 half-size))))
        (make-plane
          :normal (3d-vectors:vec3 -1 0 0)
          :distance (- (3d-vectors:vx3 center) (3d-vectors:vx3 half-size)))
        (make-plane
          :normal (3d-vectors:vec3 1 0 0)
          :distance (- (+ (3d-vectors:vx3 center) (3d-vectors:vx3 half-size)))))
      texture)))

(defstruct room-generation-config
  height
  floor-texture
  ceiling-texture
  wall-texture)

(defstruct common-generation-config
  rooms
  world-size)

(defun make-point-light-entity (&key
                                 name origin
                                 (light-center "0 0 0")
                                 (light-radius "512 512 512")
                                 (color "1 1 1")
                                 (additional-properties ()))
  (make-entity
    :classname "light"
    :name name
    :properties (append (list
                          (cons "origin" origin)
                          (cons "light_center" light-center)
                          (cons "light_radius" light-radius)
                          (cons "_color" color))
                        additional-properties)))

(defun make-player-start-entity (&key name origin (angle 0))
  (make-entity
    :classname "info_player_start"
    :name name
    :properties (append (list
                          (cons "origin" origin)
                          (cons "angle" angle)))))
