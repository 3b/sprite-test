(in-package sprite-test)


(defclass foo3 (window)
  ((maps :accessor maps :initform nil)
   (mvp :accessor mvp :initform (sb-cga:identity-matrix))
   (elements :initform 0 :accessor elements)
   (vao :initform nil :accessor vao)))

(defmethod init ((w foo3))
  (setf (gethash :main (programs w))
        (3bgl-shaders::shader-program
         :vertex 'sprite-test/shaders::vertex
         :geometry 'sprite-test/shaders::geometry
         :fragment 'sprite-test/shaders::fragment)))

(defmethod main-loop :around ((w foo3))
  (with-texture-manager ()
    (call-next-method)))

(defmethod resized :after ((w foo3))
  (setf (mvp w)
        (sb-cga:matrix*
         (kit.math:ortho-matrix 0 (wx w) (wy w) 0 1 -1)
         (sb-cga:scale* 1.0 1.0 1.0)
         (sb-cga:translate* 0.0 0.0 0.0))))

(defparameter *w* nil)

(defmethod draw ((w foo3))
  (setf *w* (list w *texture-manager*))
  (gl:clear-color 0.3 0.1 (* (abs (sin (frame-time w))) 0.2) 1.0)
  (gl:clear :color-buffer)
  (gl:disable :cull-face)
  (let ((p (gethash :main (programs w))))
    (when p
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::mvp)
            (mvp w))
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::spritesheet) 0)
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-extents) 1)
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-offsets) 2)
      (loop
        for map in (maps w)
        do (loop
             for layer in (layers map)
             do (bind-textures (first (tilesets layer)))
                (3bgl-shaders::with-program (p)
                  (gl:point-size 10)
                  (gl:line-width 10)
                  (gl:bind-vertex-array (vao layer))
                  (gl:draw-arrays :points 0 (elements layer)))))
      (gl:bind-vertex-array 0))))

(setf 3bgl-shaders::*print-shaders* t)

(defmethod key-event ((w foo3) pressed keycode keysym string)
  (when pressed
    (case keysym
      (:escape
       (exit-main-loop w))
      (:c (setf (maps w)
                (loop with *default-pathname-defaults* =
                      #p"d:/tmp/tilesets/colony/"
                      for m in (load-tmx "colony-db-map.tmx")
                      collect (apply #'make-map/tmx m))))
      (:k (setf (maps w)
                (loop with *default-pathname-defaults* =
                      #p"d:/tmp/tilesets/TileKit/"
                      for m in (load-tmx "TileKitDemo.tmx")
                      collect (apply #'make-map/tmx m)))))))


;;(run (make-instance 'foo3 :width 800 :height 1000 :x 1924 :y 32))
;;(run (make-instance 'foo3 :width 800 :height 1000 :x 4 :y 32))
