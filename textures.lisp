(in-package sprite-test)

(defclass texture ()
  ((id :reader id :initarg :id)))

(defclass 2d-texture (texture)
  ((wx :initarg :wx :reader wx)
   (wy :initarg :wy :reader wy)))

(defclass buffer-texture (texture)
  ((buffer :initarg :buffer :reader buffer)))

(defclass texture ()
  ((id :reader id :initarg :id)))

(defclass texture-manager ()
  ((textures :initform (make-hash-table :test #'equal) :reader textures)
   (parent :initform nil :reader parent :initarg :parent)))

(defvar *texture-manager*)

(defun make-texture-manager (&key inherit)
  (let ((parent (cond
                  ((and inherit (typep inherit 'texture-manager))
                   inherit)
                  ((and inherit (boundp '*texture-manager*))
                   *texture-manager*))))
    (make-instance 'texture-manager :parent parent)))

(defmethod free-texture ((tx texture))
  (gl:delete-textures (list (shiftf (slot-value tx 'id) 0))))

(defmethod free-texture ((tx buffer-texture))
  (gl:delete-buffers (list (shiftf (slot-value tx 'buffer) 0))))

(defun free-textures (m)
  (map nil 'free-texture (alexandria:hash-table-values (textures m)))
  (clrhash (textures m)))

(defmacro with-texture-manager ((&key inherit) &body body)
  `(let ((*texture-manager* (make-texture-manager :inherit ,inherit)))
     (unwind-protect
          (progn ,@body)
       (free-textures *texture-manager*))))

(defun find-texture (name &key (manager *texture-manager*))
  (when manager
    (or (gethash name (textures manager))
        (find-texture name :manager (parent manager)))))

(defun load-texture (name)
  (or (find-texture name)
      (setf (gethash name (textures *texture-manager*))
            (%load-texture name nil))))

(defun load-texture-stream (name stream type)
  (or (find-texture name)
      (setf (gethash name (textures *texture-manager*))
            (%load-texture stream type))))

(defun bind-texture (target name &key (unit 0))
  (gl:active-texture unit)
  (let ((tx (find-texture name)))
    (gl:bind-texture target (if tx (id tx) 0))))



(defun %make-buffer-texture (&key (format :rgba32f))
  (let* ((tex (gl:gen-texture))
         (buf (gl:gen-buffer)))
    (gl:bind-texture :texture-buffer tex)
    (gl:bind-buffer :texture-buffer buf)
    (%gl:tex-buffer :texture-buffer format buf)
    (gl:bind-texture :texture-buffer 0)
    (gl:bind-buffer :texture-buffer 0)
    (make-instance 'buffer-texture :id tex :buffer buf)))

(defun make-buffer-texture (name)
  (or (find-texture name)
      (setf (gethash name (textures *texture-manager*))
            (%make-buffer-texture :format :rgba32f))))

(defun bind-buffer-texture-buffer (name)
  (let ((tx (find-texture name)))
    (gl:bind-buffer :texture-buffer (if tx (buffer tx) 0))))



;;; some utilties to make it easier to get from the data structures
;;; produced by opticl to something accepted by GL

(defmacro expand-types ((object &rest types) &body body)
  `(typecase ,object
     ,@(loop for type in types
             collect `(,type
                       (locally (declare (type ,type ,object))
                         ,@body)))))

;;; NIL means pass image directly to GL, which will probably end up
;;; with it flopped since opticl considers the first pixel of data to
;;; be upper left, while GL considers it lower left.
(defparameter *flip-texture-y* t) ;; match GL coord by default

(defmacro do-pixels% ((j i wy) image &body body)
  ;; like DO-PIXELS, but with %PIXEL locally fbonud to flip the image vertically
  `(if *flip-texture-y*
       (opticl:do-pixels (,j ,i) ,image
         (macrolet ((%pixel (image-var y x)
                      `(opticl:pixel ,image-var (- ,',wy ,y 1) ,x)))
           ,@body))
       (opticl:do-pixels (,j ,i) ,image
         (macrolet ((%pixel (&rest r) `(opticl:pixel ,@r)))
           ,@body))))

(defun call-with-image-in-unsigned-bytes (image thunk)
  (cffi:with-foreign-object (p :unsigned-char (apply '* (array-dimensions image)))
    (opticl:with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                             collect `(cffi:mem-aref p :unsigned-char
                                                     (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          (1 (expand-types (image opticl:1-bit-gray-image
                                  opticl:2-bit-gray-image
                                  opticl:4-bit-gray-image
                                  opticl:8-bit-gray-image)
               (do-pixels% (j i wy) image
                 (setf (v 1) (%pixel image j i))))
           (funcall thunk p :luminance wx wy))
          (3 (expand-types (image opticl:4-bit-rgb-image
                                  opticl:8-bit-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i))))
           (funcall thunk p :rgb wx wy))
          (4 (expand-types (image opticl:4-bit-rgba-image
                                  opticl:8-bit-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))
           (funcall thunk p :rgba wx wy)))))))

(defmacro with-image-in-unsigned-bytes ((image pointer-var gl-format-varr
                                         width-var height-var)
                                        &body body)
  `(call-with-image-in-unsigned-bytes
    ,image
    (lambda (,pointer-var ,gl-format-varr ,width-var ,height-var)
      ,@body)))

(defvar *no-texture*
  (let ((image (opticl:make-8-bit-rgb-image 256 256)))
    (opticl:do-pixels (i j) image
      (if (zerop (mod (+ i j) 2))
          (setf (opticl:pixel image j i) (values 255 0 255 255))
          (setf (opticl:pixel image j i) (values 255 255 0 255))))
    image))


(defun %load-texture (name type)
  (let* ((fn (merge-pathnames name))
         (image (or (if (streamp name)
                        (opticl:read-image-stream name type)
                        (opticl:read-image-file fn))
                    *no-texture*))
         (tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)

    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)

    ;; pixelly scaling by default
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

    (with-image-in-unsigned-bytes (image p format w h)
      (gl:tex-image-2d :texture-2d 0 format w h 0 format :unsigned-byte p)
      (gl:generate-mipmap :texture-2d))
    (gl:bind-texture :texture-2d 0)
    (make-instance '2d-texture :id tex
                               :wx (array-dimension image 1)
                               :wy (array-dimension image 0))))
