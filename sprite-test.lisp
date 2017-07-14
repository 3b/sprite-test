(in-package sprite-test)


(defclass foo3 (window)
  ((maps :accessor maps :initform nil)
   (sprite-layer :accessor sprite-layer :initform nil)
   (mvp :accessor mvp :initform (sb-cga:identity-matrix))
   (sprites :accessor sprites :initform (make-array 1024 :adjustable t
                                                         :fill-pointer 0))
   (vao :initform nil :accessor vao)))


(defparameter *fc* (list nil 0))

(defmethod init ((w foo3))
  (setf (car *fc*) (now))
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

(defun update (w)
  (when (sprite-layer w)
    (let ((sprites (sprites w)))
      (loop for i across sprites
            for (x y s id) = i
            do (setf x (+ x
                          (1- (random 3))
                          (* -0.1 (/ (- x 400) 800.0))))
               (setf y (+ y
                          (1- (random 3))
                          (* -0.1 (/ (- y 500) 1000.0))))
               (setf (first i) x
                     (second i) y)
               (add-sprite (sprite-layer w)
                           i 1 id)))))

(defmethod draw ((w foo3))
  (setf *w* (list w *texture-manager*))
  (update w)
  (incf (second *fc*))
  (gl:clear-color 0.3 0.1 (* (abs (sin (frame-time w))) 0.2) 1.0)
  (gl:clear :color-buffer)
  (gl:disable :cull-face)
  (gl:point-size 10)
  (gl:line-width 10)
  (let ((p (gethash :main (programs w))))
    (when p
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::mvp)
            (mvp w))
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::spritesheet) 0)
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-extents) 1)
      (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-offsets) 2)
      (3bgl-shaders::with-program (p)
        (loop
          for map in (maps w)
          do (map nil 'draw (layers map))))
      (gl:bind-vertex-array 0))))

(setf 3bgl-shaders::*print-shaders* t)

(defmethod key-event ((w foo3) pressed keycode keysym string)
  (when pressed
    (case keysym
      (:escape
       (exit-main-loop w))
      (:f (let ((n (frame-time w)))
            (when (car *fc*)
              (let ((dt (- n (car *fc*)))
                    (c (cadr *fc*)))
                (format t "~s frames in ~6f sec = ~5f fps = ~5fms. c=~s~%"
                        c dt (/ c dt) (* 1000 (/ dt c))
                        (length (sprites w)))))
            (setf *fc* (list n 0))))
      (:c (setf (maps w)
                (loop with *default-pathname-defaults* =
                      #p"d:/tmp/tilesets/colony/"
                      for m in (load-tmx "colony-db-map.tmx")
                      collect (apply #'make-map/tmx m))))
      (:k
       (setf (maps w)
             (loop with *default-pathname-defaults* =
                   #p"d:/tmp/tilesets/TileKit/"
                   for m in (load-tmx "TileKitDemo.tmx")
                   collect (apply #'make-map/tmx m)))
       ;; allocate a dynamic sprite layer
       (setf (sprite-layer w)
             (make-instance 'dynamic-layer
                            :tilesets (tilesets (car (layers (car (maps w)))))))
       ;; and add it to map so it will be drawn. add as 2nd layer of
       ;; first (=only) map
       (push (sprite-layer w) (cdr (layers (car (maps w))))))
      (:r (setf (fill-pointer (sprites w)) 0))
      (:space
       (loop repeat 1000
             do (vector-push-extend (list (random 800)
                                          (random 1000)
                                          1
                                          (+ 194(random 3)))
                                    (sprites w)))))))

;;(run (make-instance 'foo3 :width 800 :height 1000 :x 1924 :y 32))
;;(run (make-instance 'foo3 :width 800 :height 1000 :x 4 :y 32))
