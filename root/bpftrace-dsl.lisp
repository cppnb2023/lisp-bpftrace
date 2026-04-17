(defpackage :bpftrace-dsl
  (:use :cl :generic)
  (:export :bpftrace-printf :bpftrace-progn :bpftrace-probe 
           :bpftrace-code :with-write-bpftrace :bpftrace-not-in
           :bpftrace-in))

(in-package :bpftrace-dsl)

(defun generate-occupy (keyword)
  ;;小心使用:str，因为bpftrace工具的原因没什么完美解决方案
  ;;能不用就不用，尤其是comm，username最好去proc目录去读
  ;;已经提供了读proc目录文件的函数了
  (ecase keyword
	 (:str "\\\"%s\\\"")
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
				;;((eq k :str)))
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

(defun bpftrace-if (cond then &optional else)
  (with-output-to-string (s)
	 (format s "if (~a) ~a" cond then)
	 (when else
		(format s "else ~a" else))))

(defun bpftrace-= (&rest strings)
  (when (or (not strings) (singlep strings))
	 (return-from bpftrace-= (format nil "true")))
  (do-tuple-stage-format ((left right) strings)
	 (:first (:format "(~a == ~a" left right))
	 (:main  (:format " && ~a == ~a" left right))
	 (:end   (:format ")"))))

(defun bpftrace-/= (&rest strings)
  (when (or (not strings) (singlep strings))
	 (return-from bpftrace-/= (format nil "true")))
  (do-tuple-stage-format ((left right) strings)
	 (:first (:format "(~a != ~a" left right))
	 (:main  (:format " && ~a != ~a" left right))
	 (:end   (:format ")"))))

(defmacro bpftrace-cond (&body sentence)
  (labels ((:bpftrace-cond (sentence)
				 (unless sentence (return-from :bpftrace-cond "{}"))
				 (let ((first (first sentence))
						 (rest  (rest sentence)))
					`(bpftrace-if ,(first first)
									  (bpftrace-progn
										,@(cdr first))
									  ,(:bpftrace-cond rest)))))
	 (when (nthcdr 100 sentence) 
		(error "链表太长了"))
	 (:bpftrace-cond sentence)))

(defun bpftrace-and (&rest exprs)
  (do-list-stage-format (e exprs)
	 (:first (:format "(~a" e))
	 (:main  (:format " && ~a" e))
	 (:end   (:format ")"))))

(defun bpftrace-or  (&rest exprs)
  (do-list-stage-format (e exprs)
	 (:first (:format "(~a" e))
	 (:main  (:format " || ~a" e))
	 (:end   (:format ")"))))

(defmacro bpftrace-code (&body body)
  `(macrolet ((:printf (idx &body kvlist) `(bpftrace-printf ,idx ,@kvlist))
				  (:progn (&body body)        `(bpftrace-progn ,@body))
				  (:probe (probe &body body)  `(bpftrace-probe ,probe ,@body))
				  (:not-in (var &body rest) `(bpftrace-not-in ,var ,@rest))
				  (:in (var &body rest) `(bpftrace-in ,var ,@rest))
				  (:= (&body strings) `(bpftrace-= ,@strings))
				  (:/= (&body strings) `(bpftrace-/= ,@strings))
				  (:if (cond then &optional else) `(bpftrace-if ,cond ,then ,else))
				  (:cond (&body sentence) `(bpftrace-cond ,@sentence))
				  (:and (&body exprs) `(bpftrace-and ,@exprs))
				  (:or (&body exprs) `(bpftrace-or ,@exprs))
				  (:bstr (string) `(format nil "\"~a\"" ,string))
				  (:str (var-string) `(format nil "str(~a)" ,var-string))
				  (:ustr (var-string) `(format nil "ustr(~a)" ,var-string))
				  (:t () "true")
				  (:f () "false"))
	  (bpftrace-concatenate ,@body)))

(defmacro with-write-bpftrace ((stream) &body body)
  `(format ,stream (bpftrace-code ,@body)))

