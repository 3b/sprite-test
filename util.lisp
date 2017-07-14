(in-package sprite-test)

(defun ub8->ub32 (v)
  (coerce
   (loop for i below (length v) by 4
         for x = 0
         do (loop for j below 4
                  do (setf (ldb (byte 8 (* j 8)) x)
                           (aref v (+ i j))))
         collect x)
   '(simple-array (unsigned-byte 32) (*))))

(defun decode-b64-zlib (str)
  (ub8->ub32
   (chipz:decompress nil 'chipz:zlib (base64:base64-string-to-usb8-array str))))

(defun decode-b64-gzip (str)
  (ub8->ub32
   (chipz:decompress nil 'chipz:gzip (base64:base64-string-to-usb8-array str))))

(defun translate-color (str)
  (when str
    (let ((x (parse-integer (string-left-trim '(#\#) str)
                            :radix 16))
          (c (make-array 4 :element-type 'single-float)))
      (when x
        (loop for i below 4
              do (setf (aref c i)
                       (/ (ldb (byte 8 (* i 8)) x)
                          255.0)))
        (when (<= (length str) 7)
          ;; not enough digits for AARRGGBB, set alpha to 1.0
          (setf (aref c 3) 1.0))
        c))))

(defun make-keyword (name)
  (when (and name (not (keywordp name)))
    (alexandria:make-keyword (string-upcase name))))
