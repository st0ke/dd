(in-package #:mapgen)

(defun generate-world-box (size config) ; TODO height from size or config?
  (let ((half-size (3d-vectors:v* size 0.5)))
    (make-map-content
      :brushes (list
                 ; floor
                 (make-brush-box
                   (3d-vectors:vec3 0 0 -1)
                   (3d-vectors:vec3 (3d-vectors:vx3 size) (3d-vectors:vy3 size) 2)
                   (room-generation-config-floor-texture config))
                 ; wall -x
                 (make-brush-box
                   (3d-vectors:vec3 (- 0 (3d-vectors:vx3 half-size) 1) 0 (3d-vectors:vz3 half-size))
                   (3d-vectors:vec3 2 (3d-vectors:vy3 size) (3d-vectors:vz3 size))
                   (room-generation-config-wall-texture config))
                 ; wall x
                 (make-brush-box
                   (3d-vectors:vec3 (+ (3d-vectors:vx3 half-size) 1) 0 (3d-vectors:vz3 half-size))
                   (3d-vectors:vec3 2 (3d-vectors:vy3 size) (3d-vectors:vz3 size))
                   (room-generation-config-wall-texture config))
                 ; wall -y
                 (make-brush-box
                   (3d-vectors:vec3 0 (- 0 (3d-vectors:vy3 half-size) 1) (3d-vectors:vz3 half-size))
                   (3d-vectors:vec3 (3d-vectors:vx3 size) 2 (3d-vectors:vz3 size))
                   (room-generation-config-wall-texture config))
                 ; wall y
                 (make-brush-box
                   (3d-vectors:vec3 0 (+ (3d-vectors:vy3 half-size) 1) (3d-vectors:vz3 half-size))
                   (3d-vectors:vec3 (3d-vectors:vx3 size) 2 (3d-vectors:vz3 size))
                   (room-generation-config-wall-texture config))
                 ; top
                 (make-brush-box
                   (3d-vectors:vec3 0 0 (+ (3d-vectors:vz3 size) 1))
                   (3d-vectors:vec3 (3d-vectors:vx3 size) (3d-vectors:vy3 size) 2)
                   (room-generation-config-ceiling-texture config))))))

(defparameter *default-room-config*
  (make-room-generation-config
    :height 128
    :floor-texture (make-texture-mapping-simple
                     :size (3d-vectors:vec2 64 64) :scale 0.5
                     :filepath "textures/base_floor/diamond05")
    :ceiling-texture (make-texture-mapping-simple
                     :size (3d-vectors:vec2 64 128) :scale 0.5
                     :filepath "textures/base_floor/ghotile2")
    :wall-texture (make-texture-mapping-simple
                     :size (3d-vectors:vec2 256 256) :scale 0.5
                     :filepath "textures/base_wall/rib_panel2")))

(defun generate-map-0 (config)
  (combine-map-content
    (generate-world-box
      (common-generation-config-world-size config)
      (car (common-generation-config-rooms config)))
    (make-map-content ; TODO
      :entities (list
                  (make-player-start-entity :name "start_1" :origin "0 0 0")
                  (make-point-light-entity
                    ; TODO
                    :name "light_1"
                    :origin "0 0 64"
                    :color "1 0 0")))))


(defun save-map (filepath content)
  (with-open-file (file filepath
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (progn
        (format file "Version 2~%")
        (format file "// entity 0~%")
        (format file "{~%")
        (format file "\"classname\" \"worldspawn\"~%")
        (loop
          for idx from 0
          for brush in (map-content-brushes content)
          do (progn
               (format file "// primitive ~D~%" idx)
               (format file "{~%")
               (format file "brushDef3~%")
               (format file "{~%")
               (dolist (plane (brush-planes brush))
                 (format
                   file
                   "( ~F ~F ~F ~F ) ( ( ~F ~F ~F ) ( ~F ~F ~F ) ) \"~A\" 0 0 0~%"
                   (3d-vectors:vx3 (plane-normal (brush-plane-plane plane)))
                   (3d-vectors:vy3 (plane-normal (brush-plane-plane plane)))
                   (3d-vectors:vz3 (plane-normal (brush-plane-plane plane)))
                   (plane-distance (brush-plane-plane plane))
                   (3d-vectors:vx3 (texture-mapping-transform-x (brush-plane-texture plane)))
                   (3d-vectors:vy3 (texture-mapping-transform-x (brush-plane-texture plane)))
                   (3d-vectors:vz3 (texture-mapping-transform-x (brush-plane-texture plane)))
                   (3d-vectors:vx3 (texture-mapping-transform-y (brush-plane-texture plane)))
                   (3d-vectors:vy3 (texture-mapping-transform-y (brush-plane-texture plane)))
                   (3d-vectors:vz3 (texture-mapping-transform-y (brush-plane-texture plane)))
                   (texture-mapping-filepath (brush-plane-texture plane))))
               (format file "}~%")
               (format file "}~%")))
        (format file "}~%")
        (loop
          for idx from 1
          for entity in (map-content-entities content)
          do (progn
               (format file "// entity ~D~%" idx)
               (format file "{~%")
               (format file "\"classname\" \"~A\"~%" (entity-classname entity))
               (format file "\"name\" \"~A\"~%" (entity-name entity))
               (dolist (property (entity-properties entity))
                 (format file "\"~A\" \"~A\"~%" (car property) (cdr property)))
               (format file "}~%"))))))

(defun manual-test ()
  (save-map
    "test.map"
    (generate-map-0 (make-common-generation-config
                    :rooms (list *default-room-config*)
                    :world-size (3d-vectors:vec3 512 512 128)))))

