(in-package sprite-test)

;;; tileset = texture (2d or array) + metadata buffer textures, +
;;; cpu-side metadata
;; (possibly also shader, so can have both 2d and array textures
;; transparently?)

(defclass tileset ()
  ;; names (as used in texture-manager) of textures in order of
  ;; binding point (nil for skipped bindings). Possibly should store
  ;; GL IDs directly, but shouldn't be looking them up often enough to
  ;; matter and this way we can't end up with stale IDs.
  ((texture-names :initform nil :accessor textures)
   (buffers :initform nil :accessor buffers)
   ;; general info based on tiled tileset tag
   (first-id :initform 1 :reader first-id :initarg :first-id)
   ;; most of these are probably only needed during loading since we
   ;; store tile position/size on GPU, but might as well make
   ;; available anyway
   (source :initform nil :reader source :initarg :source)
   (name :reader name :initarg :name)
   (tile-width :reader tile-width :initarg :tile-width)
   (tile-height :reader tile-height :initarg :tile-height)
   (spacing :reader spacing :initarg :spacing)
   (margin :reader margin :initarg :margin)
   (tile-count :reader tile-count :initarg :tile-count)
   (columns :reader columns :initarg :columns)
   ;;
   (tile-offset :initform #(0.0 0.0) :accessor tile-offset
                :initarg :tile-offset)
   ;; not implemented yet
   #++(transparent-color :initform nil :accessor transparent color) ;; vec3
   ;; todo: animation?
   ))

(defclass sprite-sheet-tileset (tileset)
  ;; texture-names list contains: 'tileset' texture-2d, 'tile-extents'
  ;; rgba32f buffer-texture, 'tile-offsets' rgba32f buffer-texture
  ())

#++ ;; todo
(defclass array-tileset (tileset))

(defmethod %load-tileset ((tileset sprite-sheet-tileset))
;;; todo: verify meaning of spacing/margin
  ;; assuming spacing is width of space between tiles, margin is
  ;; space around whole tileset
  (with-accessors ((twx tile-width)
                   (twy tile-height)
                   (spacing spacing)
                   (columns columns)
                   (margin margin)) tileset
    (let* ((tx (find-texture (first (textures tileset))))
           (iwx (wx tx))
           (iwy (wy tx))
           (sx (+ twx spacing))
           (sy (+ twy spacing))
           ;; extents in UV space, offsets in pixels
           (extents (make-array 4 :element-type 'single-float
                                  :adjustable t :fill-pointer 0))
           ;; possibly should store pixel offsets as integral type?
           ;; need to check version support for int buffer textures
           (offsets (make-array 4 :element-type 'single-float
                                  :adjustable t :fill-pointer 0))
           (count 0))
      (unless columns
        ;; calculate columns if not specified in file
        (setf (slot-value tileset 'columns)
              ;; assuming a file that is margin + (wx*n) would
              ;; actually have n-1 columns, since last would overlap
              ;; margin on right
              (floor (- iwx (* 2 margin)) sx)))
      ;; generate UV/offsets for tiles
      (loop with rows = (floor (- iwy (* 2 margin)) sy)
            for id from 0
            for x = (mod id columns)
            for y = (floor id columns)
            for ox = (elt (tile-offset tileset) 0)
            for oy = (elt (tile-offset tileset) 1)
            for tx = (+ 0.5 margin (* x sx))
            for ty = (+ 0.5 margin (* y sy))
            while (< y rows)
            do (incf count)
               (flet ((nx (x) (/ x (float iwx)))
                      (ny (y) (/ (- iwy y) (float iwy))))
                 (vector-push-extend (nx tx) extents)
                 (vector-push-extend (ny ty) extents)
                 (vector-push-extend (nx (+ tx (1- twx))) extents)
                 (vector-push-extend (ny (+ ty (1- twy))) extents))

               (vector-push-extend (float (+ ox 0) 1.0) offsets)
               (vector-push-extend (float (+ oy 0) 1.0) offsets)
               (vector-push-extend (float (+ ox twx) 1.0) offsets)
               (vector-push-extend (float (+ oy twy) 1.0) offsets))
      (setf (slot-value tileset 'tile-count) count)
      ;; fill metadata textures/buffers
      (bind-buffer-texture-buffer (second (textures tileset)))
      (static-vectors:with-static-vector (v (length extents)
                                            :element-type 'single-float)
        (replace v extents)
        (%gl:buffer-data :texture-buffer (* 4 (length v))
                         (static-vectors:static-vector-pointer v) :static-draw))
      (bind-buffer-texture-buffer (third (textures tileset)))
      (static-vectors:with-static-vector (v (length offsets)
                                            :element-type 'single-float)
        (replace v offsets)
        (%gl:buffer-data :texture-buffer (* 4 (length v))
                         (static-vectors:static-vector-pointer v) :static-draw))
      (gl:bind-buffer :texture-buffer 0))))

(defun load-sprite-sheet/tmx (tmx-tileset)
  (destructuring-bind (&key attributes image tile-offset
                       &allow-other-keys) tmx-tileset
    (flet ((p (n &optional (p attributes))
             (getf p n)))
      (assert (= 1 (length image)))
      (let* ((image-props (getf (first image) :attributes))
             (tileset (make-instance 'sprite-sheet-tileset
                                     :first-id (p :first-gid)
                                     :source (p :source)
                                     :name (p :name)
                                     :tile-width (p :tile-width)
                                     :tile-height (p :tile-height)
                                     :spacing (or (p :spacing) 0)
                                     :columns (p :columns)
                                     :margin (or (p :margin) 0)
                                     :tile-offset (or tile-offset #(0.0 0.0))))
             (image-source (p :source image-props)))
        (if image-source
            (load-texture image-source)
            (flex:with-input-from-sequence
                (s (p :data image-props))
              ;; use tileset object as texture name if no source file
              (load-texture-stream tileset s
                                   (make-keyword (p :format image-props)))))
        (let ((te (list tileset :tile-extents)) ;; names of buffer textures
              (to (list tileset :tile-offsets)))
          (make-buffer-texture te)
          (make-buffer-texture to)
          (setf (textures tileset) (list (or image-source tileset) te to))
          (%load-tileset tileset)
          tileset)))))

(defmethod bind-textures ((ts sprite-sheet-tileset))
  (bind-texture :texture-2d (first (textures ts)) :unit 0)
  (bind-texture :texture-buffer (second (textures ts)) :unit 1)
  (bind-texture :texture-buffer (third (textures ts)) :unit 2))
