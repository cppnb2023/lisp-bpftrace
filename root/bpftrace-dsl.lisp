(defpackage :bpftrace-dsl
  (:use :cl :generic)
  (:export :bpftrace-printf :bpftrace-progn :bpftrace-probe 
           :bpftrace-code :with-write-bpftrace :bpftrace-not-in
           :bpftrace-in :str))

(in-package :bpftrace-dsl)

(defun generate-occupy (keyword)
  (ecase keyword
;;	 (:str "\\\"%s\\\"")
	 (:i32 "%d")
	 (:u32 "%u")
	 (:i64 "%ld")
	 (:u64 "%lu")))

(defun generate-depth-string (string depth)
  (when (= depth 0)
	 (return-from generate-depth-string string))
  (flet ((-> (ch)
			  (cond
				 ((char= ch #\\) "\\\\")
				 ((char= ch #\") "\\\"")
				 (t ch))))
	 (generate-depth-string
	  (with-output-to-string (s)
		 (loop for ch across string do
				 (format s "~a" (-> ch))))
	  (1- depth))))

(defun generate-fmt (plist &key (start "") (end ""))
  (do-plist-stage-format (k v plist)
	 (:first (:format "~a:~a ~a" start v (generate-occupy k)))
	 (:main  (:format " :~a ~a" v (generate-occupy k)))
	 (:end   (:format "~a" end))))

(defun generate-args (plist &key (start "") (end ""))
  (flet ((-> (k v)
			  (cond
				;;((eq k :str)
				;; (format nil "strreplace(~a, \"~a\", \"~a\")"
				;;		 v
				;;		 (generate-depth-string "\"" 1)
				;;		 (generate-depth-string "\"" 2)))
				 (t v))))
	 (do-plist-stage-format (k v plist)
		(:first (:format "~a~a" start v))
		(:main  (:format ", ~a" (-> k v)))
		(:end   (:format "~a" end)))))

(defun bpftrace-printf (idx &rest plist)
  (format nil "printf(\"(:hash ~a ~a)\\n\" ~a)" idx
			 (generate-fmt plist)
			 (generate-args plist :start (if plist "," ""))))

(defun build-sentence (string)
  (if (or-char= (array-last string) #\{ #\} #\;)
      string (strcat string ";")))

(defun bpftrace-concatenate (&rest strings)
  (do-list-stage-format (str strings)
	 (:main (:format "~a" (build-sentence str)))))

(defun bpftrace-progn (&rest exprs)
  (format nil "{~a}" (apply #'bpftrace-concatenate exprs)))

(defun bpftrace-probe (probe &rest exprs)
  (format nil "~a{~a}" probe (apply #'bpftrace-concatenate exprs)))

(defmacro bpftrace-code (&body body)
  `(macrolet ((:printf (idx &body kvlist) `(bpftrace-printf ,idx ,@kvlist))
              (:progn (&body body)        `(bpftrace-progn ,@body))
              (:probe (probe &body body)  `(bpftrace-probe ,probe ,@body)))
     (bpftrace-concatenate ,@body)))

(defmacro with-write-bpftrace ((stream) &body body)
  `(format ,stream (bpftrace-code ,@body)))

(defun bpftrace-not-in (var &rest rest)
  (do-list-stage-format (arg rest)
	 (:first (:format "(~a != ~a" var arg))
	 (:main  (:format "&& ~a != ~a" var arg))
	 (:end   (:format ")"))))

(defun bpftrace-in (var &rest rest)
  (do-list-stage-format (arg rest)
	 (:first (:format "(~a == ~a" var arg))
	 (:main  (:format " || ~a != ~a" var arg))
	 (:end   (:format ")"))))

