(in-package sprite-test)

(defclass tile-map ()
  ((orientation :initarg :orientation :reader orientation)
   (render-order :initarg :render-order :reader render-order)
   (wx :initarg :width :reader wx)
   (wy :initarg :height :reader wy)
   (twx :initarg :tile-width :reader twx)
   (twy :initarg :tile-height :reader twy)
   ;; todo: more props from tmx tileset tag... hex stuff, staggered
   (background :initarg :background :reader background)
   (next-object-id :initarg :next-object-id :reader next-object-id)
   ;; children of tileset tag in .tmx files
   (tilesets :initarg :tilesets :reader tilesets)
   (layers :initarg :layers :reader layers)
   ;; todo: properties, objectgroup, imagelayer, group
   ))

(defun update-layer-tilesets (map)
  (let ((ranges (loop for ts in (tilesets map)
                      collect (list ts
                                    (first-id ts)
                                    (+ (first-id ts)
                                       (tile-count ts))))))
    (flet ((overlap (a b x y)
             (or (< (1- a) x b)    ;; x is in [a,b)
                 (< (1- a) y b)    ;; y is in [a,b)
                 (< (1- x) a y)))) ;; a,b in [x,y)
      (loop for l in (layers map)
            ;; fixme: do this in loader
            for nil = (when (consp (data l))
                        (assert (= 1 (length (data l))))
                        (setf (slot-value l 'data) (first (data l))))
            for (min max) = (loop for d across (data l)
                                  unless (zerop d)
                                    minimize d into min
                                    and maximize d into max
                                  finally (return (list min max)))
            do (setf (tilesets l)
                     (loop for (ts start end) in ranges
                           when (overlap start end min max)
                             collect ts))
               (unless (= 1 (length (tilesets l)))
                 ;; currently don't support more than 1 tileset used
                 ;; by a single layer, so complain (continuable since
                 ;; it will still draw something if not quite right)
                 (cerror "continue" "# of tilesets = ~s for layer ~s"
                         (length (tilesets l))
                         (name l)))
               (build-layer-geometry l)))))

(defun make-map/tmx (&key attributes tilesets layers &allow-other-keys)
  (let* ((map (apply #' make-instance 'tile-map
                        :tilesets (mapcar 'load-sprite-sheet/tmx tilesets)
                        :layers (mapcar 'make-layer/tmx layers)
                        :allow-other-keys t
                        attributes)))
    (update-layer-tilesets map)
    map))
