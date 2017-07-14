(in-package sprite-test)
;;; probably should split out tmx loader into separate system so main
;;; lib doesn't depend on XML libs if it doesn't need to

(defun xps (node)
  (let ((s (string-trim '(#\space #\tab) (xpath:string-value node))))
    (unless (string= s "") s)))

(defun attrib-hash (node)
  (let ((ret nil))
    (dom:do-node-map (a (dom:attributes node))
      (push (dom:value a) ret)
      (push (dom:local-name a) ret))
    (alexandria:plist-hash-table ret :test #'equal)))

(defun map-child-nodes (function node child-tag)
  (mapcar function (xpath:all-nodes (xpath:evaluate child-tag node))))

(defun parse-tmx-value (v type)
  (ecase type
    (:string v)
    (:int (parse-integer v))
    (:float (parse-number:parse-number v))
    (:bool (string-equal v "true"))
    (:color (translate-color v))
    (:file v)))

(defvar *attribute-parsers*
  '(:map (:version :string
          :tiled-version :string
          :orientation :keyword
          :render-order :keyword
          :width :int
          :height :int
          :tile-width :int
          :tile-height :int
          :hex-side-length :int
          :stagger-axis :keyword
          :stagger-index :keyword
          :background-color :color
          :next-object-id :int)
    :tileset (:first-gid :int
              :source :string
              :name :string
              :tile-width :int
              :tile-height :int
              :spacing :int
              :margin :int
              :tile-count :int
              :columns :int)
    :tile-offset (:x :int :y :int)
    :image (:format :keyword
            :id :int
            :source :string
            :trans :color*
            :width :int
            :height :int)
    :terrain-types ()
    :terrain (:name :string
              :tile :int)
    :tile ( ;; in layer tag:
           :gid :int
           ;; in tileset tag:
           :id :int
           :type :string ;; ?
           :terrain :csv
           :probability :float)
    :animation ()
    :frame (:tile-id :int
            :duration :int)
    :layer (:name :string
            :x :int
            :y :int
            :width :int
            :height :int
            :opacity :float
            :visible :bool
            :offset-x :int
            :offset-y :int)
    :data (:encoding :keyword
           :compression :keyword)
    :object-group (:name :string
                   :color :color
                   :x :int
                   :y :int
                   :width :int
                   :height :int
                   :opacity :float
                   :visible :bool
                   :offset-x :int
                   :offset-y :int
                   :draw-order :keyword)
    :object (:id :int
             :name :string
             :type :string
             :x :int
             :y :int
             :width :int
             :height :int
             :rotation :float
             :gid :int
             :visible :bool)
    :ellipse ()
    :polygon (:points :points)
    :polyline (:points :points)
    :text (:font-family :string
           :pixel-size :int
           :wrap :bool
           :color :color
           :bold :bool
           :italic :bool
           :underline :bool
           :strike-out :bool
           :kerning :bool
           :h-align :keyword
           :v-align :keyword)
    :image-layer (:name :string
                  :offset-x :int
                  :offset-y :int
                  :x :int
                  :y :int
                  :opacity :float
                  :visible :bool)
    :group (:name :string
            :offset-x :int
            :offset-y :int
            :x :int
            :y :int
            :opacity :float
            :visible :bool)
    :properties ()
    :property (:name :string
               :type :keyword
               :value :string)))

(defvar *node-name-map* (alexandria:plist-hash-table
                         '("tileoffset" :tile-offset
                           "terraintypes" :terrain-types
                           "objectgroup" :object-group
                           "imagelayer" :image-layer)
                         :test #'equal))
(defvar *att-name-map* (alexandria:plist-hash-table
                        '("tiledversion" :tiled-version
                          "renderorder" :render-order
                          "hexsidelength" :hex-side-length
                          "staggeraxis" :stagger-axis
                          "staggerindex" :stagger-index
                          "backgroundcolor" :background-color
                          "nextobjectid" :next-object-id
                          "firstgid" :first-gid
                          "tilewidth" :tile-width
                          "tileheight" :tile-height
                          "tilecount" :tile-count
                          "tileid" :tile-id
                          "draworder" :draw-order
                          "fontfamily" :font-family
                          "pixelsize" :pixel-size
                          "strikeout" :strike-out
                          "halign" :h-align
                          "valign" :v-align
                          "offsetx" :offset-x
                          "offsety" :offset-y)
                        :test #'equal))

(defun node-name-keyword (node)
  (let ((name (dom:local-name node)))
    (or (gethash name *node-name-map*)
        (make-keyword name))))

(defun att-name-keyword (name)
  (or (gethash name *att-name-map*)
      (make-keyword name)))

(defun parse-attributes (node)
  (let* ((name (node-name-keyword node))
         (ap (getf *attribute-parsers* name)))
    (loop for k being the hash-keys of (attrib-hash node) using (hash-value .v)
          for att = (att-name-keyword k)
          for v = (string-trim '(#\space #\tab #\return #\newline) .v)
          for type = (getf ap att)
          collect att
          collect (case type
                    ((:color :color*)
                     ;; todo: handle color/color* separately
                     ;; color assumes #(aa)rrggbb, color* might have # missing
                     (translate-color v))
                    (:csv ;; assuming only numbers in csv
                     (mapcar 'parse-integer
                             (split-sequence:split-sequence #\, v)))
                    (:points ;; x,y x,y x,y ...
                     (mapcar 'parse-integer
                             (split-sequence:split-sequence-if
                              (lambda (a) (member a '(#\space #\tab #\newline
                                                      #\return #\,)))
                              v :remove-empty-subseqs t)))
                    (:int (parse-integer v))
                    (:float (parse-number:parse-number v))
                    (:bool (not (zerop (parse-integer v))))
                    ;; todo: don't create new keywords, use
                    ;; FIND-SYMBOL and return string if that fails
                    (:keyword (make-keyword v))
                    (:string v)
                    (t
                     (if type
                         (error "don't know how to parse type ~s of attribute~
                           ~s (~s)?" type k att)
                         (error "unknown attribute ~s (~s)?" k att)))))))

(defun parse-tmx-property (prop)
  (let ((type (make-keyword (or (dom:get-attribute prop "type") :string))))
    (cons (dom:get-attribute prop "name")
          (parse-tmx-value (dom:get-attribute prop "value") type))))

(defun parse-tmx-properties (node)
  (alexandria:alist-plist
   (map-child-nodes 'parse-tmx-property node "properties/property")))

(defun parse-tmx-layer-data (data)
  (let ((encoding (make-keyword (dom:get-attribute data "encoding")))
        (comp (make-keyword (dom:get-attribute data "compression")))
        (d (string-trim '(#\space #\tab #\return #\newline)
                        (xps data))))
    (ecase encoding
      (:|| ;; <tile> children
       (coerce
        (map-child-nodes (lambda (a)
                           (parse-integer (dom:get-attribute a "gid")))
                         data "tile")
        '(simple-array (unsigned-byte 32) (*))))
      (:base64
       (ecase comp
         (:|| (ub8->ub32 (base64:base64-string-to-usb8-array d)))
         (:zlib (decode-b64-zlib d))
         (:gzip (decode-b64-gzip d))))
      (:csv
       (coerce
        (mapcar 'parse-integer (split-sequence:split-sequence #\, d))
        '(simple-array (unsigned-byte 32) (*)))))))

(defun parse-tmx-image-data (data)
  (let ((encoding (make-keyword (dom:get-attribute data "encoding")))
        (comp (make-keyword (dom:get-attribute data "compression")))
        (d (string-trim '(#\space #\tab #\return #\newline)
                        (xps data))))
    (ecase encoding ;; not sure how to interpret other encodings for images
      (:base64
       (ecase comp
         (:|| (base64:base64-string-to-usb8-array d))
         (:zlib (decode-b64-zlib d))
         (:gzip (decode-b64-gzip d)))))))

(defun parse-tmx-layer (layer)
  (let ((a (parse-attributes layer)) ;; name, opacity, visible, offsetx,offsety
        (p (parse-tmx-properties layer)))
    (list :attributes a :props p
          :data (map-child-nodes 'parse-tmx-layer-data layer "data")
          :l layer)))

(defun parse-tmx-tileoffset (n)
  (vector (parse-integer (dom:get-attribute n "x"))
          (parse-integer (dom:get-attribute n "y"))))

(defun parse-tmx-frame (n)
  (vector (parse-integer (dom:get-attribute n "x"))
          (parse-integer (dom:get-attribute n "y"))))

(defun parse-tmx-animation (n)
  (map-child-nodes 'parse-tmx-frame n "frame"))

(defun parse-tmx-terrain (n)
  (list :name (dom:get-attribute n "name")
        :tile (dom:get-attribute n "tile")
        :props (parse-tmx-properties n)))

(defun parse-tmx-terraintype (n)
  (coerce (map-child-nodes 'parse-tmx-terrain n "terrain") 'vector))

(defun parse-tmx-ts-tile (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :image (map-child-nodes 'parse-tmx-image n "image")
        :object-group (map-child-nodes 'parse-tmx-objectgroup n "objectgroup")
        :animatrion (map-child-nodes 'parse-tmx-animation n "animation")
        :tile n))

(defun parse-tmx-tileset (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :tile-offset (map-child-nodes 'parse-tmx-tileoffset n "timeoffset")
        :image (map-child-nodes 'parse-tmx-image n "image")
        :terrain-types (map-child-nodes 'parse-tmx-terraintype n "terraintypes")
        :tile (map-child-nodes 'parse-tmx-ts-tile n "tile")
        :tileset n))


(defun parse-tmx-group (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :layers  (map-child-nodes 'parse-tmx-layer n "layer")
        :object-group (map-child-nodes 'parse-tmx-objectgroup n "objectgroup")
        :image-layer (map-child-nodes 'parse-tmx-imagelayer n "imagelayer")
        :group (map-child-nodes 'parse-tmx-group n "group")
        :group n))

(defun parse-tmx-image (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :data  (map-child-nodes 'parse-tmx-image-data n "data")))

(defun parse-tmx-object (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        ;; todo
        :ellipse (map-child-nodes 'identity n "ellipse")
        :polygon (map-child-nodes 'identity n "ellipse")
        :polyline (map-child-nodes 'identity n "ellipse")
        :text (map-child-nodes 'identity n "ellipse")
        :image (map-child-nodes 'identity n "ellipse")
        :object n))

(defun parse-tmx-objectgroup (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :objects (map-child-nodes 'parse-tmx-object n "object")
        :object-group n))

(defun parse-tmx-imagelayer (n)
  (list :attributes (parse-attributes n)
        :props (parse-tmx-properties n)
        :image  (map-child-nodes 'parse-tmx-image n "image")))


(defun parse-tmx-map (map)
  (list :attributes (parse-attributes map)
        :props (parse-tmx-properties map)
        :layers  (map-child-nodes 'parse-tmx-layer map "layer")
        :tilesets (map-child-nodes 'parse-tmx-tileset map "tileset")
        :object-group (map-child-nodes 'parse-tmx-objectgroup map "objectgroup")
        :image-layer (map-child-nodes 'parse-tmx-imagelayer map "imagelayer")
        :group (map-child-nodes 'parse-tmx-group map "group")
        :map map))


(defun parse-tmx (xml)
  (let* ((map-nodes (xpath:all-nodes (xpath:evaluate "map" xml))))
    (mapcar 'parse-tmx-map map-nodes)))

(defun load-tmx (filename)
  (flet ((resolver (pubid sysid)
           (declare (ignore pubid sysid))
           (flexi-streams:make-in-memory-input-stream nil)))
    (with-open-file (f (merge-pathnames filename))
      (let* ((xml (cxml:parse-file
                   f
                   (cxml:make-namespace-normalizer
                    (cxml:make-whitespace-normalizer
                     (cxml-dom:make-dom-builder)))
                   :entity-resolver #'resolver)))
        (parse-tmx xml)))))

