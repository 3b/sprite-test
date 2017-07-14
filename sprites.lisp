(in-package sprite-test)

(defclass dynamic-layer (layer)
  ;; per-frame data for the VBOs, reset on draw
  ((vertex-data/f :initform (make-array 16384 :element-type 'single-float
                                              :adjustable t :fill-pointer 0)
                  :reader vertex-data/f)
   (vertex-data/i :initform (make-array 16384 :element-type '(unsigned-byte 32)
                                              :adjustable t :fill-pointer 0)
                  :reader vertex-data/i)))

(defmethod draw :around ((layer dynamic-layer))
  ;; for now allocating new static-vectors for copy, eventually should
  ;; allocate once and store in layer with manual reallocations on
  ;; resize. Need to set up some way to make sure they get deleted
  ;; properly first though
  (when (and (plusp (elements layer))
             (vao layer))
    (let ((count (elements layer)))
      (assert (= count
                 (length (vertex-data/i layer))
                 (/ (length (vertex-data/f layer))
                    3)))
      (static-vectors:with-static-vector (v (* 3 count)
                                            :element-type 'single-float)
        (replace v (vertex-data/f layer))
        (gl:bind-buffer :array-buffer (first (vbos layer)))
        (%gl:buffer-data :array-buffer (* 4 (* 3 count))
                         (static-vectors:static-vector-pointer v)
                         :static-draw))

      (static-vectors:with-static-vector (v count
                                            :element-type '(unsigned-byte 32))
        (replace v (vertex-data/i layer))
        (gl:bind-buffer :array-buffer (second (vbos layer)))
        (%gl:buffer-data :array-buffer (* 4 count)
                         (static-vectors:static-vector-pointer v)
                         :static-draw))))
  (unwind-protect
       (call-next-method)
    ;; reset draw lists
    (setf (elements layer) 0
          (fill-pointer (vertex-data/f layer)) 0
          (fill-pointer (vertex-data/i layer)) 0)))

(defun add-sprite (layer position scale gid)
  (ensure-buffers layer)
  (let ((vf (vertex-data/f layer))
        (vi (vertex-data/i layer))
        (base (first-id (first (tilesets layer)))))

    (vector-push-extend (float (elt position 0) 1.0) vf)
    (vector-push-extend (float (elt position 1) 1.0) vf)
    (vector-push-extend (float scale 1.0) vf)

    (vector-push-extend (- gid base) vi)

    (incf (elements layer))))

