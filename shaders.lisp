(cl:in-package sprite-test/shaders)

(input scale :float :location 1)
(input tile :int :location 2) ;; layer or index into tileset metadata

(interface varyings (:out (:vertex outs)
                     :in (:geometry (gins "ins" :*) ))
  (scale :float)
  (tile :int))

(interface varyings (:out (:geometry gouts)
                     :in (:fragment ins))
  (uv :vec3))

(defun vertex ()
  (setf gl-position position)
  (setf (@ outs scale) scale
        (@ outs tile) tile))


(uniform mvp :mat4)



(uniform spritesheet :sampler-2d)

(uniform tile-extents :sampler-buffer)
(uniform tile-offsets :sampler-buffer)

;; fragment outputs
(output color :vec4 :stage :fragment)


(defun geometry ()
  (declare (layout (:in :points) (:out :triangle-strip :max-vertices 4)))
  (symbol-macrolet ((in (aref gins 0)))
    (let* ((c (@ (aref gl-in 0) gl-position))
           (extents (texel-fetch tile-extents (@ in tile)
                                 ))
           (offsets (texel-fetch tile-offsets (@ in tile))))

      (setf (@ gouts uv) (vec3 0 0 0))

      (setf (.xy (@ gouts uv)) (.xy extents))
      (setf gl-position (* mvp (+ c (vec4 (.xy offsets) 0 0))))
      (emit-vertex)
      (setf (.xy (@ gouts uv)) (.xw extents))
      (setf gl-position (* mvp (+ c (vec4 (.xw offsets) 0 0))))
      (emit-vertex)
      (setf (.xy (@ gouts uv)) (.zy extents))
      (setf gl-position (* mvp (+ c (vec4 (.zy offsets) 0 0))))
      (emit-vertex)
      (setf (.xy (@ gouts uv)) (.zw extents))
      (setf gl-position (* mvp (+ c (vec4 (.zw offsets) 0 0))))
      (emit-vertex)
      (end-primitive))))


(defun fragment ()
  (let ((c (texture spritesheet (.xy (@ ins uv)))))
    (if (zerop (.w c))
        (discard)
        (setf color c))))
