(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(ql:quickload "parenscript")
(ql:quickload "parse-float")
(ql:quickload "local-time")

(defpackage baby-diary
  (:use :cl :cl-who :hunchentoot :parenscript))
(in-package :baby-diary)
(defparameter *h* (make-instance 'hunchentoot:easy-acceptor
			   :port 4242
			   :document-root #p"/home/pi/Documents/code/raspberry-lisp/baby-diary"))

(defparameter *db_path* "db.txt")
(defparameter *db* nil)
(defparameter *save-counter* 0)
(defconstant +diary-date-format+ (list :year #\- '(:month 2) #\- '(:day 2)))
(defconstant +diary-time-format+ (list '(:hour 2) #\: '(:min 2)))

(defun parse-request-datetime (date time)
  (local-time:timestamp-to-universal
   (local-time:parse-timestring
    (concatenate 'string date "T" time ":00")))) ;add seconds because parsing fails otherwise

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
  (main-page (:title "Sarah's diary")
    (:h1 "Sarah's diary")
    (:form
     :method :post
     :action "/submit"
     :id "input-form"
     (:div (:label "date")
	   (:input :type :date
		   :name "date"
		   :value (local-time:format-timestring nil (local-time:now) :format +diary-date-format+)
		   :class "date"))
     (:div (:label "time")
	   (:input :type :time
		   :name "time"
		   :value (local-time:format-timestring nil (local-time:now) :format +diary-time-format+)
		   :class "time"))
     (:div (:label "bv (cl)")
	   (:input :type :number
		   :name "bv"
		   :value ""))
     (:div (:label "kv (cl)")
	   (:input :type :number
		   :name "kv"
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
     (:div (:label "vitaminD")
	   (:input :type :checkbox
		   :name "vitamind"
		   :value ""))
     (:div (:label "vitaminK")
	   (:input :type :checkbox
		   :name "vitamink"
		   :value ""))
     (:div (:label "kolikin")
	   (:input :type :checkbox
		   :name "kolikin"
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
		       (dolist (field '(timestamp bv kv temperature pee poo vitaminD vitaminK kolikin weight height notes))
			 (htm (:th (fmt "~a" field))))))
	    (dolist (entry *db*)
	      (htm (:tr
		    (:td (local-time:format-timestring
			  t
			  (local-time:universal-to-timestamp (cdr (assoc 'timestamp entry)))
			  :format (concatenate 'list +diary-date-format+ (list " ") +diary-time-format+)))
		    (dolist (field '(bv kv temperature pee poo vitaminD vitaminK kolikin weight height notes))
		      (htm (:td (fmt "~@[ ~a~]" (cdr (assoc field entry))))))))))))

(define-easy-handler (submit-handler :uri "/submit")
    ;; lambda form
    ((date :parameter-type 'string)
     (time :parameter-type 'string)
     (bv :parameter-type 'integer)
     (kv :parameter-type 'integer)
     (temperature :parameter-type #'(lambda (s) (parse-float:parse-float s :junk-allowed t)))
     (weight :parameter-type #'(lambda (s) (parse-float:parse-float s :junk-allowed t)))
     (height :parameter-type 'integer)
     (poo :parameter-type 'boolean)
     (pee :parameter-type 'boolean)
     (vitaminD :parameter-type 'boolean)
     (vitaminK :parameter-type 'boolean)
     (kolikin :parameter-type 'boolean)
     (notes :parameter-type 'string))

  ;; body
  (let ((params (pairlis
		 (list 'timestamp 'bv 'kv 'temperature 'weight 'height 'pee 'poo 'vitaminD 'vitaminK 'kolikin 'notes)
		 (list
		  (parse-request-datetime date time) bv kv temperature weight height pee poo vitaminD vitaminK kolikin notes))))
    (push params *db*)
    ;; save to disk
    (incf *save-counter*)
    (when (eql *save-counter* 1)
      (with-open-file (s *db_path* :direction :output :if-exists :overwrite)
	(pprint *db* s))
      (setq *save-counter* 0))
    (format t "~{~a ~}" params)
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
