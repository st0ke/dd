(defsystem mapgen
  :author "Alexander Boldyrev <boldir@gmail.com>"
  :depends-on (:3d-vectors)
  :components ((:file "mapgen")
               (:file "types")
               (:file "labyrinth-2d")
               (:file "search-algo")
               (:file "main")))
