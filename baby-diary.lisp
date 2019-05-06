(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(ql:quickload "parenscript")
(ql:quickload "parse-float")

(defpackage baby-diary
  (:use :cl :cl-who :hunchentoot :parenscript))
(in-package :baby-diary)
(defparameter *h* (make-instance 'hunchentoot:easy-acceptor
			   :port 4242
			   :document-root #p"/home/pi/Documents/code/raspberry-lisp/baby-diary"))

(defparameter *db_path* "db.txt")
(defparameter *db* nil)
(defparameter *save-counter* 0)

(defmacro main-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:meta :http-equiv "Content-Type"
	      :content "text/html;charset=utf-8")
       (:title ,title)
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/www/main.css"))
      (:body
       ,@body))))

(define-easy-handler (main-handler :uri "/") (name)
  (main-page (:title "baby diary")
    (:h1 "baby diary")
    (:form
     :method :post
     :action "/submit"
     :id "input-form"
     (:div (:label "food (cl)")
	   (:input :type :number
		   :name "food"
		   :value ""))
     (:div (:label "temperature (°C)")
	   (:input :type :number
		   :name "temperature"
		   :value ""
		   :step 0.1))
     (:div (:label "pee")
	   (:input :type :checkbox
		   :name "pee"
		   :value ""))
     (:div (:label "poo")
	   (:input :type :checkbox
		   :name "poo"
		   :value ""))
     (:div (:label "weight (kg)")
	   (:input :type :number
		   :name "weight"
		   :value ""
		   :step 0.1))
     (:div (:label "height (cm)")
	   (:input :type :number
		   :name "height"
		   :value ""
		   :step 0.1))
     (:div (:label "notes")
	   (:input :type :text
		   :name "notes"
		   :value ""))
     (:input :type :submit
	     :value "submit"))
    (:table :id "data"
	    (htm :tr (let ((entry (first *db*)))
		       (dolist (field '(timestamp food temperature pee poo weight height notes))
			 (htm (:th (fmt "~a" field))))))
	    (dolist (entry *db*)
	      (htm (:tr
		    (multiple-value-bind
			  (second minute hour date month year day-of-week dst-p tz)
			(decode-universal-time (cdr (assoc 'timestamp entry)))
		      (htm (:td (fmt "~2,'0d.~2,'0d.~4,'0d ~2,'0d:~2,'0d" date month year hour minute))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'food entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'temperature entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'pee entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'poo entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'weight entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'height entry))))
		    (:td (fmt "~@[ ~a~]" (cdr (assoc 'notes entry))))))))))

(define-easy-handler (submit-handler :uri "/submit")
    ;; lambda form
    ((food :parameter-type 'integer)
     (temperature :parameter-type #'(lambda (s) (parse-float:parse-float s :junk-allowed t)))
     (weight :parameter-type #'(lambda (s) (parse-float:parse-float s :junk-allowed t)))
     (height :parameter-type 'integer)
     (poo :parameter-type 'boolean)
     (pee :parameter-type 'boolean)
     (notes :parameter-type 'string))

  ;; body
  ;; TODO this should be created with a macro
  (let ((params (pairlis
		 (list 'timestamp 'food 'temperature 'weight 'height 'pee 'poo 'notes)
		 (list (get-universal-time) food temperature weight height pee poo notes))))
    (push params *db*)
    ;; save to disk
    (incf *save-counter*)
    (when (eql *save-counter* 1)
      (with-open-file (s *db_path* :direction :output :if-exists :overwrite)
	(pprint *db* s))
      (setq *save-counter* 0))
    (format t "~{~a ~}" *db*)
    (redirect "/")))

(push (hunchentoot:create-static-file-dispatcher-and-handler "/www/main.css" "./www/main.css") *dispatch-table*)

(defun stop-server ()
  (when (and *h* (started-p *h*))
    (hunchentoot:stop *h*)))

(defun start-server ()
  (progn
    (stop-server)
    (setq *db* (with-open-file
		   (s *db_path* :if-does-not-exist :create)
		 (read s nil nil)))
    (hunchentoot:start *h*)))

(defun reset-database ()
  (setq *db* nil))
