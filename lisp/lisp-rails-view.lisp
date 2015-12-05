(defpackage :lisp-rails-view
  (:use :cl))

(in-package :lisp-rails-view)

(defvar *buffer* nil)

(defclass raw ()
  ((value :initarg :value :accessor value)))

(defclass raw= (raw) ())

(defun raw (x)
  (make-instance 'raw :value x))

(defun raw= (x)
  (make-instance 'raw= :value x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-reader (&body body)
    `(let ((*readtable* (copy-readtable nil)))
       (setf (readtable-case *readtable*) :preserve)
       ,@body)))

(defun call (template)
  (with-reader (%call template))
  (flush-buffer t))

(defun %call (template)
  (with-open-file (in template)
    (loop for form = (read in nil nil)
          while form
          do (%eval form))))

(defun flush-buffer (stream)
  ;;TODO constant の連結
  (loop for x in (nreverse *buffer*)
        do (format stream "~a~%" (to-ruby-exp x))))

(defun emit (x)
  (push x *buffer*))

(defun to-ruby-token (x)
  (typecase x
    (string x
     (with-output-to-string (out)
       (write-char #\" out)
       (loop for c across (princ-to-string x)
             do (cond ((char= c #\")
                       (write-string "\\\"" out))
                      (t
                       (write-char c out))))
              (write-char #\" out)))
    (t x)))

(defmethod to-ruby-exp (x)
  (with-output-to-string (out)
    (write-string "b__.push(\"" out)
    (loop for c across (princ-to-string x)
          do (cond ((char= c #\")
                    (write-string "\\\"" out))
                   (t
                    (write-char c out))))
    (write-string "\")" out)))

(defmethod to-ruby-exp ((x number))
  (to-ruby-exp (princ-to-string x)))

(defmethod to-ruby-exp ((x raw))
  (value x))

(defmethod to-ruby-exp ((x raw=))
  (format nil "b__.push(~a)" (value x)))


(defmethod %eval (x)
  (emit x))

(defmethod %eval ((x null)))

(defmethod %eval ((x cons))
  (cond ((eq '= (car x))
         (emit (raw= (make-ruby-form (cdr x) t))))
        ((keywordp (car x))
         (emit (format nil "<~a>" (car x)))
         (loop for i in (cdr x)
               do (%eval i))
         (emit (format nil "</~a>" (car x))))
        ((and (symbolp (car x))
              (fboundp (car x)))
         (emit (eval x)))
        (t
         (emit (raw (make-ruby-form x))))))

(defun blockp (form)
  (and (consp (car (last form)))
       (string-equal "lambda" (caar (last form)))))

(defun make-ruby-form (form &optional new-buffer-p)
  (with-output-to-string (out)
    (let* ((blockp (blockp form))
           (block-form (and blockp (cdar (last form))))
           (main-form (if blockp (butlast form) form)))
      (make-ruby-main-form (car main-form) (cdr main-form) out)
      (when block-form
        (format out "{~a}" (make-ruby-block block-form new-buffer-p))))))

(defmethod make-ruby-main-form (first rest out)
  (format out "~a" (to-ruby-token first))
  (loop for i in rest
        do (if (consp i)
               (format out "(~{~a~^, ~})" (mapcar #'make-ruby-form (list i)))
               (progn
                 (write-char #\. out)
                 (format out "~a" (to-ruby-token i))))))

(defmethod make-ruby-main-form ((first (eql '|if|)) rest out)
  (format out "if ~a~%~aelse~%~aend"
          (make-ruby-form (car rest))
          (make-ruby-block-body (ensure-list (cadr rest)))
          (make-ruby-block-body (ensure-list (caddr rest)))))

(defun make-ruby-block (form &optional new-buffer-p)
  (with-output-to-string (out)
    (let ((args (car form))
          (body (make-ruby-block-body (cdr form))))
      (when args
        (format out "|~{~a~^, ~}|~%" args))
      (if new-buffer-p
          (format out "
[].tap { |b__|
~a
}.flatten.join.html_safe~%" body)
          (format out "~a" body)))))

(defun make-ruby-block-body (form)
  (let ((*buffer* nil))
    (loop for i in form
          do (%eval i))
    (with-output-to-string (out)
      (flush-buffer out))))

(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))

(let ((template (cadr sb-ext:*posix-argv*)))
  (when template
    (call template)))
