(in-package #:cl-user)
(defpackage #:mapgen
  (:use #:cl #:3d-vectors)
  (:export
    #:generate-map
    #:save-map))
