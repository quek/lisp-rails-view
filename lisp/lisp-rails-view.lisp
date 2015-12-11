(defpackage :lisp-rails-view
  (:use :cl)
  (:shadow :#=))

(in-package :lisp-rails-view)

(defvar *buffer* nil)

(defclass ruby-code ()
  ((value :initarg :value :accessor value)))

(defclass =ruby-code (ruby-code) ())

(defclass html-safe ()
  ((value :initarg :value :accessor value)))

(defun ruby-code (x)
  (make-instance 'ruby-code :value x))

(defun =ruby-code (x)
  (make-instance '=ruby-code :value x))

(defun html-safe (x)
  (make-instance 'html-safe :value x))

(defun escape (thing)
  (with-output-to-string (out)
    (loop for c across (princ-to-string thing)
          do (cond ((char= #\& c)
                    (write-string "&amp;" out))
                   ((char= #\< c)
                    (write-string "&lt;" out))
                   ((char= #\> c)
                    (write-string "&gt;" out))
                   ((char= #\" c)
                    (write-string "&quot;" out))
                   ((char= #\' c)
                    (write-string "&#x27;" out))
                   (t (write-char c out))))))

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
  (let (buffer)
    (labels ((<< (x)
               (setf buffer
                     (if buffer
                         (html-safe
                          (concatenate 'string (value buffer)
                                       (if (stringp x)
                                           (escape x)
                                           (value x))))
                         (if (stringp x)
                             (html-safe (escape x))
                             x))))
             (flush ()
               (when buffer
                 (%write buffer)
                 (setf buffer nil)))
             (%write (x)
               (format stream "~a~%" (to-ruby-exp x))))
      (loop for i in (nreverse *buffer*)
            do (typecase i
                 ((or string html-safe) (<< i))
                 (t
                  (flush)
                  (%write i))))
      (flush))))

(defun emit (x)
  (push x *buffer*))

(defun to-s (x)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across (princ-to-string x)
          do (cond ((char= c #\")
                    (write-string "\\\"" out))
                   (t
                    (write-char c out))))
    (write-char #\" out)))

(defun to-ruby-token (x)
  (typecase x
    (string x
     (to-s x))
    (t x)))

(defmethod to-ruby-exp (x)
  (with-output-to-string (out)
    (write-string "b__.push(" out)
    (write-string (to-s x) out)
    (write-string ")" out)))

(defmethod to-ruby-exp ((x number))
  (to-ruby-exp (princ-to-string x)))

(defmethod to-ruby-exp ((x ruby-code))
  (value x))

(defmethod to-ruby-exp ((x =ruby-code))
  (format nil "b__.push(~a)" (value x)))

(defmethod to-ruby-exp ((x html-safe))
  (format nil "b__.push(~a.html_safe)" (to-s (value x))))


(defmethod %eval (x)
  (emit x))

(defmethod %eval ((x null)))

(defmethod %eval ((x cons))
  (cond ((eq '= (car x))
         (emit (=ruby-code (make-ruby-form (cdr x) t))))
        ((keywordp (car x))
         (process-tag x))
        ((and (symbolp (car x))
              (fboundp (car x)))
         (emit (eval x)))
        (t
         (emit (ruby-code (make-ruby-form x))))))

(defun process-tag (form)
  (multiple-value-bind (tag id classes) (parse-tag (car form))
    (multiple-value-bind (attributes body /-p) (parse-tag-args (cdr form))
      (when classes
        (let ((kv (assoc "class" attributes :test #'string=)))
          (when kv
            (setf (cdr kv) `(append ',classes
                                    (ensure-list ,(cdr kv)))
                  classes nil))))
      (emit (html-safe (with-output-to-string (out)
                         (format out "<~a" tag)
                         (when id
                           (format out " id=\"~a\"" (escape id)))
                         (when classes
                           (format out " class=\"~{~a~^ ~}\"" (mapcar #'escape classes)))
                         (loop for (k . v) in attributes do
                           (if (constantp v)
                               (if (eq v t)
                                   (format out " ~a" (escape k))
                                   (format out " ~a=\"~a\"" (escape k) (escape v)))
                               (format out "#{~a == true ? \" ~a\" : \" ~a=\"~a\"\"}" v k k v)))
                         (if /-p
                             (write-string " />" out)
                             (write-string ">" out)))))
      (loop for i in body
            do (%eval i))
      (emit (html-safe (format nil "</~a>" tag))))))

(defun parse-tag (tag)
  (let* ((str (string-downcase (symbol-name tag)))
         (p# (position #\# str))
         (p. (position #\. str)))
    (cond ((and (not p#) (not p.))
           str)
          ((and p# (not p.))
           (values (subseq str 0 p#)
                   (subseq str (1+ p#))))
          ((and (not p#) p.)
           (values (subseq str 0 p.)
                   nil
                   #1=(loop for start = (1+ p.) then (1+ end)
                            for end = (position #\. str :start start)
                            collect (subseq str start end)
                            unless end
                              do (loop-finish))))
          (t
           (values (subseq str 0 p#)
                   (subseq str (1+ p#) p.)
                   #1#)))))

(defun parse-tag-args (args)
  (let (attributes)
    (labels ((f (args)
               (if (and (consp args)
                        (keywordp (car args)))
                   (progn
                     (push (cons (string-downcase (car args)) (cadr args)) attributes)
                     (f (cddr args)))
                   (values (nreverse attributes)
                           args
                           (if (atom args)
                               args
                               (cdr (last args)))))))
      (f args))))

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
[].tap do |b__|
def b__.push(x)
  if x.is_a?(Array)
    x.map do |y|
      push(y)
    end
  else
    super(x.html_safe? ? x : ERB::Util.h(x))
  end
end
~a
end.flatten.join.html_safe~%" body)
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
