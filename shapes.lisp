(in-package :pantzer)

(declaim (optimize (speed 3)))

(defgeneric get-side (shape index))
(defgeneric get-vertex (shape index &optional camera))
(defgeneric (setf get-vertex) (new shape index &optional camera))
(defgeneric draw-shape (shape))
(defgeneric update-cam-vertexes (shape))
(defgeneric find-side (shape ix))
(defgeneric get-centre (shape &optional camera ))
(defgeneric move (object &optional distance))
(defgeneric turn (object angle))

(defvar *skip-zapping-cam-state* nil)

;; Utility function
(defun random-pick (seq)
  (let ((n (random (length seq))))
    (elt seq n)))


(defclass shape ()
  ((vertexes :reader vertexes :initarg :vertexes)
   (cam-vertexes :reader cam-vertexes :initarg :cam-vertexes)
   (needs-updating :accessor needs-updating :initarg :needs-updating :initform t)
   ))

(defclass volume (shape)
  ((sides :reader sides :initarg :sides)
   (to-show :accessor to-show :initarg :to-show) 
   ))

(defclass sphere (shape)
  ())

(defclass flat (shape)
  ())

(defclass triangle (flat)
  ())
(defclass tetragon (flat)
  ())

(defmethod move ((cam camera) &optional (distance 1.0d0))
  (let (
	;(angle (+ (angle cam) (/ pi 2.0)))
	(angle (angle cam))
	)
    (declare (type (double-float -20.0d0 20.0d0) angle)
	     (type double-float distance))
    (incf (x cam) (* distance (sin angle)))
    (incf (y cam) (* distance (cos angle)))))

(defmethod turn ((cam camera) angle)
  (let ((tmp (angle cam)))
    (declare (double-float tmp angle))
    (setf (angle cam) (mod (+ angle tmp) (* pi 2.0d0)))))

(defmethod get-centre ((shape sphere) &optional camera)
  (update-cam-vertexes shape)
  (get-vertex shape 0 camera))

(defmethod get-centre ((shape volume) &optional camera)
  (update-cam-vertexes shape)
  (let ((vexes (if camera (cam-vertexes shape) (vertexes shape)))
	(avg-coord (make-instance 'coord :x 0.0d0 :y 0.0d0 :z 0.0d0)))
    (loop for coord across vexes
	  do (incf (x avg-coord) (the-x coord))
	  do (incf (y avg-coord) (the-y coord))
	  do (incf (z avg-coord) (the-z coord)))
    (let ((len (length vexes)))
      (setf (x avg-coord) (/ (the-x avg-coord) len))
      (setf (y avg-coord) (/ (the-y avg-coord) len))
      (setf (z avg-coord) (/ (the-z avg-coord) len)))
    avg-coord))

(defmacro defvolume (name vertexes sides &optional show-limit &rest extra-slots)
  "(DEFVOLUME <shape> <vertexes>
  ((<side-type> <vertex1> ...)
   ...) [<extra slot definition> ...])

Volumes should be defined with vertexes clockwise, from bottom upwards.
The sides should have vertexes in clock-wise order as seen from outside
the volume, looking along a normal to the face.

Sides should be in some order, though ideally ''will never be seen'' surfaces (tops, bottoms) should be placed last."
  (let ((maker (intern (format nil "MAKE-~a" (string-upcase name)) *pantzer-package*))
	(maker-args (loop for cnt from 0 below vertexes
			  collect (intern (format nil "S~d" cnt) *pantzer-package*)))
	(side-vec (coerce sides 'vector)))
    `(progn
       (defclass ,name (volume)
	 ,extra-slots
	 (:default-initargs :vertexes (make-array ,vertexes) :sides ',side-vec))
       (defun ,maker ,maker-args
	 (let ((vexes (make-array ,vertexes))
	       (cam-vexes (make-array ,vertexes)))
	   ,@(loop for cnt from 0 below vertexes
		   collect `(setf (aref cam-vexes ,cnt)
				   (make-instance 'coord))
		   collect `(setf (aref vexes ,cnt)
				  ,(find-symbol (format nil "S~d" cnt)
						*pantzer-package*)))
	   (make-instance ',name
			  :vertexes vexes
			  :cam-vertexes cam-vexes
			  :to-show ,show-limit)))
       ,@(loop for (side . vertexes) in sides
	       for n from 0
	       collect `(defmethod get-side ((shape ,name) (side (eql ,n)))
			  (let ((vexes (make-array ,(length vertexes)))
				(cv (make-array ,(length vertexes))))
			    ,@(loop for n in vertexes
				   for count from 0
				   collect `(setf (aref vexes ,count)
						  (get-vertex shape ,n))
				   collect `(setf (aref cv ,count)
						  (get-vertex shape ,n t)))
			    (make-instance ',side
					   :vertexes vexes
					   :cam-vertexes cv)))))))


(defvolume tetraeder 4 ((triangle 0 1 3) (triangle 1 2 3) (triangle 2 0 3) (triangle 0 1 2)) 3)
(defvolume box 8 ((tetragon 0 1 5 4) (tetragon 1 2 6 5) (tetragon 2 3 7 6) (tetragon 3 0 4 7) (tetragon 3 2 1 0) (tetragon 4 5 6 7)) 6)
(defvolume octaeder 6 ((triangle 0 2 1) (triangle 0 3 2) (triangle 0 4 3) (triangle 0 1 4) (triangle 2 5 1) (triangle 3 5 2) (triangle 4 5 3) (triangle 1 5 4)) 8)
(defvolume tank 14 ((tetragon 0 4 7 3) (tetragon 0 1 5 4) (tetragon 1 2 6 5)
		    (tetragon 2 3 7 6) (triangle 4 8 7) (tetragon 4 5 9 8)
		    (triangle 5 6 9) (tetragon 7 13 12 6) (tetragon 7 8 10 13)
		    (tetragon 8 9 11 10) (tetragon 9 11 6 12)
		    (tetragon 0 1 2 3) (tetragon 10 11 12 13)) 11
		    (pos :accessor pos :initarg :pos) 
		    (world-vertexes :accessor world-vertexes :initarg :world-vertexes)
		    (camera :accessor camera :initarg :camera)
		    (turn-left :accessor turn-left :initform nil)
		    (reload-timer :accessor reload-timer :initform 20) 
		    )

(defmethod get-vertex ((shape tank) ix &optional camera)
  (let ((base (aref (vertexes shape) ix))
	(cam (camera shape))
	(offset (pos shape)))
    (let ((rv (world-to-camera base cam (aref (world-vertexes shape) ix))))
      (incf (x rv) (the-x offset))
      (incf (y rv) (the-y offset))
      (if camera (world-to-camera rv *camera* rv) rv))))

(defmethod get-centre ((shape tank) &optional camera)
  (let ((rv (pos shape)))
    (if camera (world-to-camera rv *camera*)
      rv)))

(defmethod x ((tank tank))
  (the-x (pos tank)))
(defmethod y ((tank tank))
  (the-y (pos tank)))
(defmethod z ((tank tank))
  (the-z (pos tank)))
(defmethod angle ((tank tank))
  (angle (camera tank)))

(defmethod move ((tank tank) &optional (distance 0.8d0))
  (let ((angle (angle (camera tank)))
	(pos (pos tank)))
    (declare (double-float distance angle))
    (tank-ai tank)
    (incf (x pos) (* distance (sin angle)))
    (incf (y pos) (* distance (cos angle)))))

(defun tank-ai (tank)
  (let ((coord (coord (the-x *camera*) (the-y *camera*) 0.0d0))
	(cam (make-instance 'camera
			    :x (the-x tank) :y (the-y tank) :z (the-z tank)
			    :angle (angle tank))))
    
    (world-to-camera coord cam coord)
    (decf-if-positive (reload-timer tank))
    (let ((y (the-y coord))
	  (x (the-x coord)))
      (cond ((<= (abs x) y)
	     (turn tank (* (cond ((< x 0.0d0) -1.0d0)
				 ((> x 0.0d0)  1.0d0)
				 (t (random-pick #(-1.0d0 1.0d0))))
			   (cond ((<= (abs x) (/ y 3))
				  (when (and (<= y 150)
					     (zerop (reload-timer tank)))
				    (setf (reload-timer tank)
					  (fire :shooter tank
						:life-time 180)))
				  0.01d0)
				 (t 0.05d0)))))
	    (t (when (zerop (random 10))
		 (setf (turn-left tank)
		       (not (turn-left tank))))
	       (turn tank (if (turn-left tank) -0.1d0 0.1d0)))))))

(defmethod turn ((tank tank) angle)
  (turn (camera tank) angle))

(defclass missile (box)
  ((delta :reader delta :initarg :delta)
   (rounds :accessor rounds :initarg :rounds)
   (shooter :reader shooter :initarg :shooter) 
   ))

(defmethod move ((obj missile) &optional distance)
  (declare (ignore distance))
  (let ((dx (the-x (delta obj)))
	(dy (the-y (delta obj)))
	(dz (the-z (delta obj))))
    (loop for v across (vertexes obj)
	  do (incf (x v) dx)
	  do (incf (y v) dy)
	  do (incf (z v) dz))
    (decf (rounds obj))
    (when (<= (rounds obj) 0)
      (setf *all-shapes* (delete obj *all-shapes*))
      (setf *moving-objects* (delete obj *moving-objects*)))))
  

(defmethod update-cam-vertexes ((shape volume))
  (setf (needs-updating shape) nil)
  (loop for n from 0
	for source across (vertexes shape)
	do (setf (aref (cam-vertexes shape) n)
		 (world-to-camera source
				  *camera*
				  (aref (cam-vertexes shape) n)))))

(defmethod update-cam-vertexes ((shape tank))
  (setf (needs-updating shape) nil)
  (let ((cam-vertexes (cam-vertexes shape)))
    (loop for n from 0
	  for source across cam-vertexes
	  do (setf (aref cam-vertexes n) (get-vertex shape n t)))))

(defmethod get-vertex ((shape shape) ix &optional camera)
  (when (and camera (needs-updating shape))
    (update-cam-vertexes shape))
  (if (and (<= 0 ix)
	   (< ix (length (vertexes shape))))
      (aref (if camera (cam-vertexes shape) (vertexes shape)) ix)
    (error "~S is not a valid vertex for volume ~S." ix shape)))

(defmethod (setf get-vertex) (value (shape shape) ix &optional camera)
  (setf (needs-updating shape) t)
  (let ((vexes (if camera (cam-vertexes shape) (vertexes shape))))
    (setf (aref vexes ix) value)))

(defmethod (setf x) :before (val (shape camera))
  (unless *skip-zapping-cam-state*
    (loop for s in *all-shapes*
	  do (setf (needs-updating s) t))))
(defmethod (setf y) :before (val (shape camera))
  (unless *skip-zapping-cam-state*
    (loop for s in *all-shapes*
	  do (setf (needs-updating s) t))))
(defmethod (setf z) :before (val (shape camera))
  (unless *skip-zapping-cam-state*
    (loop for s in *all-shapes*
	  do (setf (needs-updating s) t))))
(defmethod (setf angle) :before (val (shape camera))
  (unless *skip-zapping-cam-state*
    (loop for s in *all-shapes*
	  do (setf (needs-updating s) t))))
(defmethod (setf focal) :before (val (shape camera))
  (unless *skip-zapping-cam-state*
    (loop for s in *all-shapes*
	  do (setf (needs-updating s) t))))

(defun y-average (shape &optional (cam t))
  (let ((data (if cam (cam-vertexes shape) (vertexes shape))))
    (/ (reduce #'+ data :key 'y) (length data))))

(defun in-view (cam-coord)
  (and (<= 0.0d0 (the-y cam-coord) 200.0d0)
       (<= (sqrt (+ (square (the-x cam-coord)) (square (the-z cam-coord))))
	   (the-y cam-coord))
       ))

(defmethod find-side ((shape volume) ix)
  (let ((side (get-side shape ix)))
    (when (some 'in-view (cam-vertexes side))
      (list side))))

(defmethod draw-shape ((shape volume))
  (let ((max (min (to-show shape) (length (sides shape)))))
    (unless (typep shape 'tank)
      (update-cam-vertexes shape))
    (let ((sides (sort (loop for n from 0 below max
			     append (find-side shape n))
		       #'>
		       :key 'y-average)))
      (loop for side in sides
	   do (draw-shape side)))))


(defun spin-vertexes (vexes)
  (let ((vertexes (coerce vexes 'list)))
    (setf (cdr (last vertexes)) vertexes)
    (labels ((step-test (v)
	       (if (in-view (car v))
		   (loop for check across vexes
			 for vex in v
			 collect v)
		 (step-test (cdr v)))))
      (step-test vertexes))))

(defun line-start (x1 y1 x2 y2)
  (cond ((and (<= 0 x1 (1- +screen-side+))
	      (<= 0 y1 (1- +screen-side+)))
	 (list x1 y1))
	((and (< x1 0) (< x2 0))
	 nil)
	((and (< y1 0) (< y2 0))
	 nil)
	((and (>= x1 +screen-side+) (>= x2 +screen-side+))
	 nil)
	((and (>= y1 +screen-side+) (>= y2 +screen-side+))
	 nil)
	((= x1 x2)
	 (cond ((< y1 0) (list x1 0))
	       ((>= y1 +screen-side+) (list x1 (1- +screen-side+)))))
	((= y1 y2)
	 (cond ((< x1 0) (list 0 y1))
	       ((>= x1 +screen-side+)
		(list (1- +screen-side+) y1)))) 
	(t
	 (let ((dx (- x2 x1))
	       (dy (- y2 y1)))
	   (let ((k (/ dy dx)))
	     (let ((xcut (cond ((< x1 0) 0)
			       ((<= 0 x1 +screen-side+) x1)
			       ((<= +screen-side+ x1) (1- +screen-side+)))))
	       (let ((y0 (+ (* k (- xcut x1)) y1)))
		 (let ((ycut (cond ((< y0 0)
				    0)
				   ((<= 0 y0 +screen-side+)
				    y0)
				   ((<= +screen-side+ y0)
				    (1- +screen-side+)))))
		   (let ((x0 (+ (/ (- ycut y0) k) xcut)))
		     (list x0 ycut))))))))))

(defun bresenwalk (x1 y1 x2 y2)
  "Do a Bresenham Line Drawing from <x1 y1> to <x2 y2>, starting outside
visible screen, stop once inside, return (xN yN), that being the first
coordinates inside visible space."
  (when (and (or (/= (signum x1)
		     (signum x2))
		 (= (* (signum (- x1 +screen-side+))
		       (signum (- x2 +screen-side+)))
		    -1)
		 (and (<= 0 x1 (1- +screen-side+))
		      (<= 0 x2 (1- +screen-side+))))
	     (or (/= (signum y1)
		     (signum y2))
		 (= (* (signum (- y1 +screen-side+))
		       (signum (- y2 +screen-side+)))
		    -1)
		 (and (<= 0 y1 (1- +screen-side+))
		      (<= 0 y2 (1- +screen-side+))))) 
    (let ((dx (- x2 x1))
	  (dy (- y2 y1)))
      (cond ((< (abs dx) (abs dy))
	     (let ((y-off (/ dx 2))
		   (xN x1)
		   (yN y1))
	       (loop until (and (<= 0 xn (1- +screen-side+))
				(<= 0 yn (1- +screen-side+)))
		     for n below 30
		     do (progn
			  (incf yn (signum dy))
			  (incf y-off dx)
			  (when (>= (abs y-off) (abs dy))
			    (incf xn (signum dx))
			    (decf y-off dy))))
	       (list xn yn)))
	    ((>= (abs dx) (abs dy))
	     (let ((x-off (/ dy 2))
		   (xN x1)
		   (yN y1))
	       (loop until (and (<= 0 xn (1- +screen-side+))
				(<= 0 yn (1- +screen-side+)))
		     do (progn
			  (incf xn (signum dx))
			  (incf x-off dy)
			  (when (>= (abs x-off) (abs dx))
			    (incf yn (signum dy))
			    (decf x-off dx))))
	       (list xn yn)))))))

(defun clip-coord-line (start finish)
  "This function operates in CAMERA SPACE"
  (let ((dx (- (the-x finish) (the-x start)))
	(dy (- (the-y finish) (the-y start)))
	(dz (- (the-z finish) (the-z start))))
    (if (zerop dx)
	(coord (the-x start) (abs (the-x start)) (the-z start))
      (let* ((k (/ dy dx))
	     (m (- (the-y start) (* k (the-x start)))))
	(cond ((= k 1.0d0)
	       (let ((nx (- (/ m (1- k))))
		     (ny (- (/ m (1- k)))))
		 (coord nx
			ny
			(+ (the-z start)
			   (* dz (/ (sqrt (+ (* dx dx) (* dy dy)))
				    (sqrt (+ (square (- nx (the-x start)))
					     (square (- ny (the-y start)))))))))))
	      ((= k -1.0d0)
	       (let ((nx (- (/ m (1+ k))))
		     (ny (/ m (1+ k))))
		 (coord nx
			ny
			(+ (the-z start)
			   (* dz (/ (sqrt (+ (* dx dx) (* dy dy))) 
				    (sqrt (+ (square (- nx (the-x start)))
					     (square (- ny (the-y start)))))))))))
	      (t (let* ((cx1 (- (/ m (1- k))))
			(cx2 (- (/ m (1+ k))))
			(cy1 cx1)
			(cy2 (- cx2)))
		   (flet ((compute-z (nx ny)
			    (+ (the-z start)
			       (* dz
				  (/ (sqrt (+ (* dx dx) (* dy dy))) 
				     (sqrt (+ (square (- nx (the-x start)))
					      (square (- ny (the-y start))))))))))
		   (cond ((and (< cy1 -0.0d0) (< cy2 -0.0d0))
			  nil)
			 ((< cy1 -0.0d0)
			  (coord cx2 cy2 (compute-z cx2 cy2)))
			 ((< cy2 -0.0d0)
			  (coord cx1 cy1 (compute-z cx1 cy1)))
			 (t (let ((d1 (+ (square (- cx1 (the-x start)))
					 (square (- cy1 (the-y start)))))
				  (d2 (+ (square (- cx2 (the-x start)))
					 (square (- cy2 (the-y start))))))
			      (if (< d1 d2)
				  (coord cx1 cy1 (compute-z cx1 cy1))
				(coord cx2 cy2 (compute-z cx2 cy2))))))))))))))

(defun build-partially-clipped (vertexes &optional collector last)
  (cond ((null vertexes) (apply #'append (nreverse collector)))
	((null collector) ; First vertex!
	 (build-partially-clipped (cdr vertexes)
				  (cons
				   (camera-to-screen (car vertexes) *camera*)
				   collector)
				  (car vertexes)))
	((and (in-view last) (in-view (car vertexes)))
	 (build-partially-clipped (cdr vertexes)
				  (cons
				   (camera-to-screen (car vertexes) *camera*)
				   collector)
				  (car vertexes)))
	((and (in-view last) (not (in-view (car vertexes))))
	 (let* ((sp (camera-to-screen last *camera*))
		(halfmeasure (clip-coord-line (car vertexes) last))
		(ep (and halfmeasure (camera-to-screen halfmeasure *camera*))))
	   (if halfmeasure
	       (build-partially-clipped (cdr vertexes)
					(cons (bresenwalk (car ep) (cadr ep)
							  (car sp) (cadr sp))
					      collector)
					(car vertexes))
	     (build-partially-clipped (cdr vertexes)
				      collector
				      (car vertexes)))))
	 
	((and (not (in-view last)) (in-view (car vertexes)))
	 (let* ((halfmeasure (clip-coord-line last (car vertexes)))
		(sp (and halfmeasure (camera-to-screen halfmeasure *camera*)))
		(ep (camera-to-screen (car vertexes) *camera*)))
	   (if halfmeasure
	       (build-partially-clipped (cdr vertexes)
					(cons (bresenwalk (car sp) (cadr sp)
							  (car ep) (cadr ep))
					  collector)
				    (car vertexes))
	     (build-partially-clipped (cdr vertexes)
				      collector
				      (car vertexes)))))
	((and (not (in-view last)) (not (in-view (car vertexes))))
	 (let* ((halfsp (clip-coord-line last (car vertexes)))
		(sp (and halfsp (camera-to-screen halfsp *camera*)))
		(halfep (clip-coord-line (car vertexes) last))
		(ep (and halfep (camera-to-screen halfep *camera*))))
	   (if (and halfsp halfep)
	       (build-partially-clipped (cdr vertexes)
					(cons (bresenwalk (car ep) (cadr ep)
							  (car sp) (cadr sp))
					      (cons (bresenwalk (car sp) (cadr sp)
								(car ep) (cadr ep))
						    collector))
					(car vertexes))
	     (build-partially-clipped (cdr vertexes)
				      collector
				      (car vertexes)))))))

(defun draw-all-shapes ()
  (let ((shapes (loop for shape in *all-shapes*
		      do (update-cam-vertexes shape)
		      if (some 'in-view (cam-vertexes shape))
		      collect shape)))
    (loop for shape in (sort shapes #'> :key #'y-average)
	  do (draw-shape shape))))

(defun place-tetraeder (centre side &optional (angle 0.0d0))
  (let* ((z-min (- (the-z centre) (* side 0.2041241452319315d0)))
	 (z-max (+ (the-z centre) (* side 0.6123724356957945d0)))
	 (height (* side (sqrt 0.75d0)))
	 (d (/ height 3.0d0)))

    (let ((cam (make-instance 'camera :x 0.0d0 :y 0.0d0 :z 0.0d0 :focal 1.0d0 :angle angle))
	  (c1 (coord (* side -0.5d0) (- d) 0.0d0))
	  (c2 (coord 0.0d0 (- height d) 0.0d0))
	  (c3 (coord (* side 0.5d0) (- d) 0.0d0)))
      (let ((c1 (world-to-camera c1 cam c1))
	    (c2 (world-to-camera c2 cam c2))
	    (c3 (world-to-camera c3 cam c3)))
	(loop for corner in (list c1 c2 c3)
	      do (progn
		   (incf (x corner) (the-x centre))
		   (incf (y corner) (the-y centre))
		   (setf (z corner) z-min)))
	(push
	 (make-tetraeder c1 c2 c3 (coord (the-x centre) (the-y centre) z-max))
	 *all-shapes*)))))

(defun place-box (centre side &optional (angle 0.0d0))
  (let ((delta (/ side 2.0d0)))
    (let ((cam (make-instance 'camera
			      :x 0.0d0 :y 0.0d0 :z 0.0d0
			      :focal 1.0d0 :angle angle))
	  (c1 (coord (- delta) (- delta) (- delta)))
	  (c2 (coord (- delta) (+ delta) (- delta)))
	  (c3 (coord (+ delta) (+ delta) (- delta)))
	  (c4 (coord (- delta) (+ delta) (- delta)))
	  (c5 (coord (- delta) (- delta) (+ delta)))
	  (c6 (coord (- delta) (+ delta) (+ delta)))
	  (c7 (coord (+ delta) (+ delta) (+ delta)))
	  (c8 (coord (- delta) (+ delta) (+ delta))))
      (let ((c1 (world-to-camera c1 cam c1))
	    (c2 (world-to-camera c2 cam c2))
	    (c3 (world-to-camera c3 cam c3))
	    (c4 (world-to-camera c4 cam c4))
	    (c5 (world-to-camera c5 cam c5))
	    (c6 (world-to-camera c6 cam c6))
	    (c7 (world-to-camera c7 cam c7))
	    (c8 (world-to-camera c8 cam c8)))
	(loop for vertex in (list c1 c2 c3 c4 c5 c6 c7 c8)
	      do (progn
		   (incf (x vertex) (the-x centre))
		   (incf (y vertex) (the-y centre))
		   (incf (z vertex) (the-z centre))
		   ))
	(push (make-box c1 c2 c3 c4 c5 c6 c7 c8)
	      *all-shapes*)))))

(defun place-octaeder (centre side &optional (angle 0.0d0))
  (let ((delta (* side (sqrt 0.5d0))))
    (let ((cam (make-instance 'camera
			      :x 0.0d0 :y 0.0d0 :z 0.0d0
			      :focal 1.0d0 :angle angle))
	  (c1 (coord    0.0d0     0.0d0  (- delta)))
	  (c2 (coord    delta     0.0d0     0.0d0))
	  (c3 (coord    0.0d0  (- delta)    0.0d0))
	  (c4 (coord (- delta)    0.0d0     0.0d0))
	  (c5 (coord    0.0d0     delta     0.0d0))
	  (c6 (coord    0.0d0     0.0d0     delta)))
      (let ((c1 (world-to-camera c1 cam c1))
	    (c2 (world-to-camera c2 cam c2))
	    (c3 (world-to-camera c3 cam c3))
	    (c4 (world-to-camera c4 cam c4))
	    (c5 (world-to-camera c5 cam c5))
	    (c6 (world-to-camera c6 cam c6)))
	(loop for vertex in (list c1 c2 c3 c4 c5 c6)
	      do (progn
		   (incf (x vertex) (the-x centre))
		   (incf (y vertex) (the-y centre))
		   (incf (z vertex) (the-z centre))
		   ))
	(push (make-octaeder c1 c2 c3 c4 c5 c6)
	      *all-shapes*)))))
	    

(defun place-tank (coord angle) 
  (let ((c1  (coord 4.6875    1.875 0.0))
	(c2  (coord 4.6875   -1.875 0.0)) 
	(c3  (coord -1.875   -1.875 0.0))
	(c4  (coord -1.875    1.875 0.0))
	(c5  (coord 5.625     2.8125 0.5))
	(c6  (coord 5.625    -2.8125 0.5))
	(c7  (coord -2.8125  -2.8125 0.5))
	(c8  (coord -2.8125   2.8125 0.5))
	(c9  (coord 1.875     1.40625 1.875))
	(c10 (coord 1.875    -1.40625 1.875))
	(c11 (coord 1.40625   1.40625 2.8125))
	(c12 (coord 1.40625  -1.40625 2.8125))
	(c13 (coord -1.40625 -1.40625 2.8125))
	(c14 (coord -1.40625  1.40625 2.8125)))
    (let ((tank (make-tank c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14)))
      (setf (pos tank)
	    coord)
      (setf (camera tank)
	    (make-instance 'camera
			   :x 0.0d0 :y 0.0d0 :z 0.0d0
			   :focal 1.0d0 :angle angle))
      (let ((wv (make-array 14 :element-type 'coord :initial-element (coord 0.0d0 0.0d0 0.0d0))))
	(loop for n from 1 below 14
	      do (setf (aref wv n)
		       (coord 0.0d0 0.0d0 0.0d0)))
	(setf (world-vertexes tank) wv))
      (push tank *all-shapes*)
      (push tank *moving-objects*)
      tank)))

(defun emit-tetraeder (x y side &optional (angle (random (/ pi 3.0d0))))
  (car (place-tetraeder (coord x y (* side 0.2041241452319315d0))
			side
			angle)))

(defun emit-box (x y side &optional (angle (random (/ pi 2))))
  (car (place-box (coord x y (/ side 2.0d0)) side angle)))

(defun emit-octaeder (x y side &optional (angle (random (/ pi 2))))
  (car (place-octaeder (coord x y (* side (sqrt 0.5d0))) side angle)))

(defun emit-tank (x y radius &optional (angle (random (/ pi 2))))
  (declare (ignore radius))
  (place-tank (coord x y 1.5d0) angle))

(defgeneric collide-p (o1 o2))
(defgeneric collide-action (o1 o2))

(defun score-from-object (obj)
  (let ((type (type-of obj)))
    (or (cdr (assoc type *score-table*))
	0)))

(defun lowest (axis seq)
  (reduce #'min seq :key axis))

(defun highest (axis seq)
  (reduce #'max seq :key axis))

(defmethod collide-p ((o1 camera) o2)
  (let ((min-x (lowest #'x (vertexes o2)))
	(max-x (highest #'x (vertexes o2))))
    (when (<= min-x (the-x o1) max-x)
      (let ((min-y (lowest #'y (vertexes o2)))
	    (max-y (highest #'y (vertexes o2))))
	(when (<= min-y (the-y o1) max-y)
	  (format t ";;; Collision detected~%")
	  t)))))

(defmethod collide-p (o1 o2)
  (unless (eql o1 o2)
    (let ((min-x1 (lowest #'x (vertexes o1)))
	  (max-x1 (highest #'x (vertexes o1)))
	  (min-x2 (lowest #'x (vertexes o2)))
	  (max-x2 (highest #'x (vertexes o2))))
      (when (typep o1 'tank)
	(incf min-x1 (the-x (pos o1)))
	(incf max-x1 (the-x (pos o1))))
      (when (typep o2 'tank)
	(incf min-x2 (the-x (pos o2)))
	(incf max-x2 (the-x (pos o2))))
      (when (and (<= min-x1 max-x2)
		 (<= min-x2 max-x1))
	(let ((min-y1 (lowest #'y (vertexes o1)))
	      (max-y1 (highest #'y (vertexes o1)))
	      (min-y2 (lowest #'y (vertexes o2)))
	      (max-y2 (highest #'y (vertexes o2))))
	  (when (typep o1 'tank)
	    (incf min-y1 (the-y (pos o1)))
	    (incf max-y1 (the-y (pos o1))))
	  (when (typep o2 'tank)
	    (incf min-y2 (the-y (pos o2)))
	    (incf max-y2 (the-y (pos o2))))
	  (when (and (<= min-y1 max-y2)
		     (<= min-y2 max-y1))
	    t))))))

(defmethod collide-p ((o1 tank) (o2 tank))
  ;;; NEEDS ACCURACY
  (unless (eql o1 o2)
    (when (< (distance (pos o1) (pos o2)) 10)
      t)))

(defmethod collide-action ((o1 missile) o2)
  (unless (or (eql o1 o2)
	      (eql (shooter o1) o2))
    (setf (rounds o1) -1)
    (setf *all-shapes* (delete o2 *all-shapes*))
    (setf *moving-objects* (delete o2 *moving-objects*))
    (unless (shooter o1)
      (incf *score* (score-from-object o2)))))

(defmethod collide-action ((o1 tank) o2)
  (setf *all-shapes* (delete o1 *all-shapes*))
  (setf *moving-objects* (delete o1 *moving-objects*)))

(defmethod collide-action ((o1 tank) (o2 tank))
  (setf *all-shapes* (delete o1 *all-shapes*))
  (setf *moving-objects* (delete o1 *moving-objects*))
  (setf *all-shapes* (delete o2 *all-shapes*))
  (setf *moving-objects* (delete o2 *moving-objects*)))

(defmethod collide-action ((o1 tank) (o2 missile))
  (collide-action o2 o1))

(defmethod collide-action (o1 o2)
  (format t "~&;;; Generic collision handler entered for ~a hitting ~a." o1 o2))

(defmethod collide-action ((o1 camera) o2)
  (loop while (collide-p o1 o2)
	do (move o1 -1.0d0)))

(defmethod collide-action ((o1 camera) (o2 missile))
  (when (shooter o2)
    (setf *moving-objects* (delete o2 *moving-objects*))
    (setf *all-shapes* (delete o2 *all-shapes*))
    (decf *lives*)))
    
(defmethod collide-action ((o1 missile) (o2 camera))
  (collide-action o2 o1))

;;; Object generation follows

(defun ran (b r &optional pos-only)
  (* (if pos-only
	 1
       (aref #(-1 1) (random 2)))
     (+ b (random (float r 0.0d0)))))

(defun gen-objects (n &optional (emitters (vector #'emit-box #'emit-tetraeder #'emit-octaeder)))
  (loop for cnt below n 
	do (funcall (random-pick emitters)
		    (+ (the-x *camera*) (ran 20 150))
		    (+ (the-y *camera*) (ran 20 150))
		    (ran 2 3 t))))

(defun gen-objects-if-needed ()
  (let ((tanks (reduce #'(lambda (n obj)
			   (if (typep obj 'tank)
			       (1+ n)
			     n))
		       *moving-objects* :initial-value 0))
	(obstacles (reduce #'(lambda (n obj)
			       (if (not (or (typep obj 'missile)
					    (typep obj 'tank)))
				   (1+ n)
				 n))
			   *all-shapes* :initial-value 0)))
    (when (<= tanks 2)
      (gen-objects 10 (vector #'emit-tank)))
    (when (<= obstacles 20)
      (gen-objects 50))))
