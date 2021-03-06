(in-package sprite-test)

(defclass map-layer (layer)
  ((data :initarg :data :reader data)
   (offset :initarg :offset :reader offset)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)))


(defun make-layer/tmx (tmx-layer)
  (destructuring-bind (&key attributes props data
                       &allow-other-keys) tmx-layer
    (declare (ignore props))
    (apply 'make-instance
           'map-layer
           :data data
           :offset (vector (or (getf attributes :x) 0.0)
                           (or (getf attributes :y) 0.0))
           :tilesets nil
           attributes)))

(defun build-layer-geometry (layer)
  ;; need to have at least 1 tileset, more than 1 not supported
  ;;(assert (= 1 (length (tilesets layer))))
  (assert (tilesets layer))
  (ensure-buffers layer)
  (let* ((q nil)
         (map (data layer))
         (ts (first (tilesets layer)))
         (base (first-id ts))
         (mwx (width layer))
         (twx (tile-width ts))
         (twy (tile-height ts))
         (count 0))
    (setf q (loop with ox = (elt (offset layer) 0)
                  with oy = (elt (offset layer) 1)
                  for i below (length map)
                  for x = (+ ox (* (mod i mwx) twx))
                  ;; is oy in correct direction?
                  for y = (+ oy (* (floor i mwx) twy))
                  unless (zerop (aref map i))
                    collect (float x 1.0)
                    and collect (float y 1.0)
                    and collect 1.0
                    and do (incf count)))
    (static-vectors:with-static-vector (v (length q)
                                          :element-type 'single-float)
      (replace v q)
      (gl:bind-buffer :array-buffer (first (vbos layer)))
      (%gl:buffer-data :array-buffer (* 4 (* 3 count))
                       (static-vectors:static-vector-pointer v)
                       :static-draw))

    (static-vectors:with-static-vector (v count
                                          :element-type '(unsigned-byte 32))
      (loop with i = 0
            for id across map
            unless (zerop id)
              do (assert (< i (length v)))
                 (setf (aref v i) (- id base))
                 (incf i))
      (gl:bind-buffer :array-buffer (second (vbos layer)))
      (%gl:buffer-data :array-buffer (* 4 count)
                       (static-vectors:static-vector-pointer v)
                       :static-draw))
    (setf (elements layer) count)))
