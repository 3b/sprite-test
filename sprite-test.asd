(defsystem :sprite-test
  :description "2d sprite engine test"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria opticl cl-opengl glop 3bgl-shader chipz cl-base64
                          cxml xpath cxml-stp split-sequence static-vectors
                          sb-cga mathkit)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "shaders")
               (:file "backend")
               (:file "textures")
               (:file "tileset")
               (:file "sprite-layer")
               (:file "map-layer")
               (:file "sprites")
               (:file "map")
               (:file "tmx")
               (:file "sprite-test")))
