(defpackage :bpftrace-dsl
  (:use :cl :base-tools :dsl)
  (:export #:eval-bpftrace #:defun-bpftrace #:defmacro-bpftrace
           #:macroexpand-1-bpftrace #:macroexpand-bpftrace
           #:funcall-bpftrace #:apply-bpftrace #:bpftrace-gensym
           #:bpftrace-expand))

(in-package :bpftrace-dsl)

;;这里需要一个type-system，先用symbol-system顶替

(define-symbol-system *bpftrace*)

(defun bpftrace-transform (codes)
  (if (symbolp codes)
      (string-downcase (symbol-name codes))
      codes))

(defun bpftrace-expand (codes)
  (eval-bpftrace codes #'bpftrace-transform))

(defun bpftrace-gensym (&optional global-p (preffix "b"))
  (gensym (format nil "~:[$~;@~]~a" global-p preffix)))

(defun-bpftrace :set (var val)
  (format nil "~a = ~a" var val))

(defun-bpftrace :progn (&rest strings)
  (do-complex ((:format fmt)) ((:list code strings))
    (:start (fmt "{"))
    (:do (fmt "~a"
              (if (or-char= (array-last code) #\{ #\} #\;)
                  code
                  (concatenate 'string code ";"))))
    (:finally (fmt "}"))))

(defmacro-bpftrace :let ((&rest bindings) &body body)
  `(:progn
     ,@(loop for (var val) in bindings
             collect `(:set ,var ,val))
     ,@body))

(defun-bpftrace :if (cond then &optional else)
  (with-output-to-string (stream)
    (format stream "if (~a) ~a" cond then)
    (when else
      (format stream "else ~a" else))))

(defun-bpftrace :gethash (key hash)
  (format nil "~a[~a]" key hash))

(defmacro with-bpftrace-symbols (symbols &body body)
  `(let ,(loop for s in symbols
               collect `(,s (bpftrace-gensym)))
     ,@body))

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

(defun-bpftrace :printf% (fmt &rest args)
  (format nil "printf(\"~a\"~{, ~a~})"
          (generate-string fmt 1)
          args))

(defun-bpftrace :strlen (var)
  (format nil "strlen(~a)" var))

(defmacro-bpftrace :printf (idx-sym &rest parameters)
  (declare (keyword idx-sym))
  (multiple-value-bind (bind args fmt)
      (do-complex ((:collect bind arg) (:format fmt))
          ((:list (k p v) parameters))
        (:start (fmt "(:IDX ~s" idx-sym))
        (:do (fmt " ~s ~a"
                  (the keyword k)
                  (make-placeholder (the keyword p)))
             (if (eq p :str)
                 (with-bpftrace-symbols (tmp)
                   (bind `(,tmp ,v))
                   (arg  `(:strlen ,tmp) tmp))
                 (arg v)))
        (:finally (fmt ")")))
    `(:let ,bind
       (:printf% ,fmt ,@args))))

(define-dsl-usually-op *bpftrace*)
