(defpackage :bpftrace-dsl
  (:use :cl :base-tools :dsl)
  (:export #:with-bpftrace-expand #:bpftrace-expand #:bpftrace-gensym
           #:define-bpftrace-setf-expander #:get-bpftrace-setf-expansion))

(in-package :bpftrace-dsl)

(define-symbol-system bpftrace)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-dsl-symbol (symbol)
    (string-downcase (substitute #\_ #\- (string symbol))))

  (defun bpftrace-expand (code)
    (cond
      ((typep code 'string)
       (format nil "~s" code))
      ((typep code 'number)
       (format nil "~a" code))
      ((typep code 'symbol)
       (aif2 (macroexpand code)
             (bpftrace-expand it)
             (cond
               ((eq code nil) "null")
               ((eq code t)   "true")
               (t (to-dsl-symbol code)))))
      ((typep code 'list)
       (get-bpftrace-expansion code))
      (t (error (format nil "unknown type ~a" (type-of code)))))))

(defmacro defmacro-bpftrace (name parameters &body body)
  `(define-bpftrace-expander ,name ,parameters
     (bpftrace-expand (progn ,@body))))

(define-dsl-expander :lang-bpftrace (&body body)
  (bpftrace-expand body))

(defmacro with-bpftrace-expand ((&rest vars) &body body)
  `(let ,(loop for v in vars collect `(,v (bpftrace-expand ,v)))
     ,@body))

(defun bpftrace-gensym (&optional global-p (preffix "b"))
  (gensym (format nil "~:[$~;@~]~a" global-p preffix)))

(define-bpftrace-expander :set (var val)
  (with-bpftrace-expand (var val)
    (format nil "~a = ~a" var val)))

(define-bpftrace-expander :progn (&body body)
  (do-complex ((:format fmt)) ((:list code body))
    (:start (fmt "{"))
    (:do (fmt "~a"
              (let ((code (bpftrace-expand code)))
                (if (or-char= (array-last code) #\{ #\} #\;)
                    code
                    (concatenate 'string code ";")))))
    (:finally (fmt "}"))))

(defmacro-bpftrace :let ((&rest bindings) &body body)
  `(:progn
     ,@(loop for (var val) in bindings
             collect `(:set ,var ,val))
     ,@body))

(define-bpftrace-expander :if (cond then &optional else)
  (with-bpftrace-expand (cond then)
    (with-output-to-string (stream)
      (format stream "if (~a) ~a" cond then)
      (when else
        (format stream "else ~a" (bpftrace-expand else))))))

(define-bpftrace-expander :define-probe (probe cond &body body)
  (with-bpftrace-expand (probe cond)
    (format nil "~a/~a/~a" probe cond
            (bpftrace-expand `(:progn ,@body)))))

(define-bpftrace-expander :gethash (key hash)
  (with-bpftrace-expand (key hash)
    (format nil "~a[~a]" key hash)))

(define-dsl-operator symbol-bpftrace-expander bpftrace-expand
  (:binary-compute-operator
   (:+ "~a"   "+")
   (:- "-~a"  "-")
   (:* "~a"   "*")
   (:/ "1/~a" "/")
   (:logior "~a"   "|")
   (:logand "~a"   "&")
   (:logxor "~~~a" "^"))

  (:transitivity-compare
   (:=  "==")
   (:<  "<")
   (:>  ">")
   (:<= "<=")
   (:>= ">="))

  (:untransitivity-compare
   (:/= "!=")))

(defmacro with-bpftrace-symbols (symbols &body body)
  `(let ,(loop for s in symbols
               collect `(,s (bpftrace-gensym)))
     ,@body))

(define-symbol-system bpftrace-setf)

(defun bpftrace-setf-expansion (place)
  (with-symbols (new)
    (if (symbolp place)
        (list nil nil new `(:set ,place ,new) place)
        (aif2 (get-bpftrace-setf-expansion place)
              it
              (list nil nil new `(:set ,place ,new) place)))))

(define-bpftrace-setf-expander :gethash (key hash)
  (with-bpftrace-symbols (key-sym hash-sym new)
    (list (list key-sym hash-sym)
          (list key hash)
          (list new)
          `(:set (:gethash ,key-sym ,hash-sym) ,new)
          hash-sym)))

(defmacro-bpftrace :setf (&rest parameters)
  `(:progn
     ,@(do-complex ((:collect clt)) ((:plist a b parameters))
         (:do (destructuring-bind (bind val new set read)
                  (bpftrace-setf-expansion a)
                (declare (ignorable read))
                (clt `(:let (,@(mapcar #'list bind val)
                             (,(first new) ,b))
                        ,set)))))))

(defun make-placeholder (symbol)
  (ecase symbol
    (:u32 "%u")
    (:u64 "%lu")
    (:i32 "%d")
    (:i64 "%ld")
    (:str "{%u %s")))

(set-macro-character
 #\{
 (lambda (stream ch)
   (declare (ignorable ch))
   (with-output-to-string (s)
     (loop repeat (read stream)
           do (format s "~c" (read-char stream))))))

(define-bpftrace-expander :printf (idx-sym &rest parameters)
  (multiple-value-call #'format nil
    (format nil "{~~{~~a;~~}printf(\"(:idx ~s~~{ ~~s ~~a~~})\"~~{, ~~a~~})}"
            (the symbol idx-sym))
    (do-complex ((:collect pre plc arg)) ((:list (k p v) parameters))
      (:do (plc (the keyword k) (make-placeholder (the keyword p)))
        (if (equal p "%s")
            (let ((tmp (bpftrace-gensym)))
              (pre (bpftrace-expand `(:set ,tmp ,v)))
              (arg tmp))
            (arg (bpftrace-expand v)))))))
