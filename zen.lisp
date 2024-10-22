(in-package #:zen)

(defun skip-reader (stream char)
  (declare (ignore char))
  (read stream t nil t)
  (values))

(set-macro-character #\~ #'skip-reader)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro let-gensym (specs &body body)
  `(let (,@(loop :for spec :in specs
                 :collect (if (symbolp spec)
                              `(,spec (gensym ,(string spec)))
                              (let* ((sym (car spec))
                                     (options (cdr spec))
                                     (name (getf options :name sym))
                                     (count (getf options :count)))
                                `(,sym ,(if count
                                            `(loop :repeat ,count
                                                   :collect (gensym ,(string name)))
                                            `(gensym ,(string name))))))))
     ,@body))

(defun curry (func &rest args)
  (lambda (&rest rest) (apply func (append args rest))))

(defmacro curry1 (func &rest args)
  (let-gensym (x)
    `(lambda (,x) (,func ,@args ,x))))

(defun iota (n)
  (loop :for i :from 0 :below n :collect i))

(defmacro alterf (&rest args &environment env)
  `(progn
     ,@(loop :for (place partial-expr) :on args :by #'cddr
             :collect
             (multiple-value-bind (temp-vars temp-forms store-vars store-form access-form)
                 (get-setf-expansion place env)
               (let ((access-vars (loop :repeat (length store-vars) :collect (gensym))))
                 `(let (,@(loop :for var :in temp-vars
                                :for form :in temp-forms
                                :collect `(,var ,form)))
                    (multiple-value-bind ,access-vars ,access-form
                      (let (,@(loop :for store-var :in store-vars
                                    :for access-var :in access-vars
                                    :collect `(,store-var (funcall ,partial-expr ,access-var))))
                        ,store-form))))))))

(defmacro defplace (name args &body body)
  (let-gensym (env)
    `(progn
       (defun ,name ,args
         ,(eval `(let ,(loop :for arg :in args :collect `(,arg ',arg)) ,@body)))
       (define-setf-expander ,name (,@args &environment ,env)
         (get-setf-expansion (progn ,@body) ,env)))))

(defmacro defglobal (name value)
  `(defconstant ,name
     (if (boundp ',name)
         ,name
         ,value)))

(eval-always
  (defun pattern-arity (pattern)
    (if (consp pattern)
        (case (car pattern)
          (values (length (cdr pattern)))
          (and (loop :for subpattern :in (cdr pattern)
                     :maximize (pattern-arity subpattern)))
          (t 1))
        1)))

(eval-always
  (defun compile-form-pattern (pattern form env body)
    (multiple-value-bind (temp-vars temp-forms store-vars store-form access-form)
        (get-setf-expansion form env)
      `(let (,@(loop :for temp-var :in temp-vars
                     :for temp-form :in temp-forms
                     :collect `(,temp-var ,temp-form)))
         ,(compile-place-pattern pattern store-vars store-form access-form env body)))))

(eval-always
  (defun compile-place-pattern (pattern store-vars store-form access-form env body)
    (cond ((and (consp pattern) (eq (car pattern) 'ref))
           (let-gensym (get-value)
             `(flet ((,get-value () ,access-form)
                     ((setf ,get-value) (,(car store-vars)) ,store-form))
                (declare (inline ,get-value (setf ,get-value))
                         (ignorable (function ,get-value) (function (setf ,get-value))))
                (symbol-macrolet ((,(cadr pattern) (,get-value)))
                  ,body))))
          ((eq pattern t) body)
          ((and (consp pattern) (eq (car pattern) 'and))
           (compile-and-pattern (cdr pattern) store-vars store-form access-form env body))
          ((and (consp pattern) (eq (car pattern) 'guard))
           (destructuring-bind (test) (cdr pattern)
             `(when ,test ,body)))
          (t (let ((values (loop :repeat (pattern-arity pattern)
                                 :collect (gensym (string 'scrutinee)))))
               `(multiple-value-bind (,@values) ,access-form
                  ,(compile-value-pattern pattern values env body)))))))

(eval-always
  (defun compile-value-pattern (pattern values env body)
    (let ((value (car values)))
      (cond ((null pattern)
             `(when (null ,value)
                ,body))
            ((eq t pattern)
             body)
            ((numberp pattern)
             `(when (= ,value ,pattern)
                ,body))
            ((keywordp pattern)
             `(when (eq ,value ,pattern)
                ,body))
            (t
             (case (car pattern)
               (var
                 (destructuring-bind (name) (cdr pattern)
                   `(let ((,name ,value)) ,body)))
               (values
                 (compile-tuple-pattern
                   (loop :for subpattern :in (cdr pattern)
                         :for value :in values
                         :collect (cons subpattern value))
                   env
                   body))
               (quote
                 (destructuring-bind (literal) (cdr pattern)
                   `(when (eq ,value ',literal)
                      ,body)))
               (member
                 `(when (member ,value (list ,@(cdr pattern)))
                    ,body))
               (equalp
                 (destructuring-bind (other) (cdr pattern)
                   `(when (equalp ,value ,other)
                      ,body)))
               (cons
                 (destructuring-bind (head tail) (cdr pattern)
                   `(when (consp ,value)
                      ,(compile-form-pattern
                         head `(car ,value) env
                         (compile-form-pattern
                           tail `(cdr ,value) env
                           body)))))
               (the
                 (destructuring-bind (type subpattern) (cdr pattern)
                   `(when (typep ,value ',type)
                      ,(compile-value-pattern subpattern (list value) env body))))
               (satisfies
                 (destructuring-bind (pred) (cdr pattern)
                   `(when (,(if (atom pred) `(,pred ,value) `(,@pred ,value)))
                      ,body)))
               (of
                 (destructuring-bind (func subpattern) (cdr pattern)
                   (compile-form-pattern
                     subpattern
                     (if (atom func) `(,func ,value) `(,@func ,value))
                     env
                     body)))
               (guard
                 (destructuring-bind (test) (cdr pattern)
                   `(when ,test ,body)))
               (and
                 (compile-tuple-pattern
                   (loop :for subpattern :in (cdr pattern) :collect (cons subpattern value))
                   env
                   body))
               (otherwise
                 (let* ((macro-func (get (car pattern) 'pattern-macro))
                        (expanded-pattern (apply macro-func (cdr pattern))))
                   (compile-value-pattern expanded-pattern values env body)))))))))

(eval-always
  (defun compile-and-pattern (patterns store-vars store-form access-form env body)
    (loop :for pattern :in (reverse patterns)
          :do (setf body (compile-place-pattern pattern store-vars store-form access-form env body))
          :finally (return body))))

(eval-always
  (defun compile-tuple-pattern (components env body)
    (loop :for (pattern . value) :in (reverse components)
          :do (setf body (compile-value-pattern pattern (list value) env body))
          :finally (return body))))

(defmacro defpattern (name args &body body)
  (setf (get name 'pattern-macro)
        (eval `(lambda (,@args) ,@body))))

(defpattern list (&rest patterns)
  (loop :with list-pattern = nil
        :for pattern :in (reverse patterns)
        :do (setf list-pattern `(cons ,pattern ,list-pattern))
        :finally (return list-pattern)))

(defpattern list* (patterns tail)
  (loop :with list-pattern = tail
        :for pattern :in (reverse patterns)
        :do (setf list-pattern `(cons ,pattern ,list-pattern))
        :finally (return list-pattern)))

(define-condition match-error (error) ())

(defmacro match (scrutinee &rest cases &environment env)
  (multiple-value-bind (temp-vars temp-forms store-vars store-form access-form)
      (get-setf-expansion scrutinee env)
    (let-gensym (match-block)
      `(let (,@(loop :for temp-var :in temp-vars
                     :for temp-form :in temp-forms
                     :collect `(,temp-var ,temp-form)))
         (block ,match-block
           ,@(loop :for (pattern . body) :in cases
                   :collect (compile-place-pattern
                              pattern store-vars store-form access-form env
                              `(return-from ,match-block (progn ,@body)))))))))

(defmacro bind (pattern scrutinee &body body)
  `(match ,scrutinee (,pattern ,@body)))

(defmacro bind* (bindings &body body)
  (loop :with result = `(progn ,@body)
        :for (pattern scrutinee) :in (reverse bindings)
        :do (setf result `(bind ,pattern ,scrutinee ,result))
        :finally (return result)))

(eval-always
  (defun var-reader (stream char)
    (declare (ignore char))
    `(var ,(read stream t nil t))))

(eval-always
  (defun ref-reader (stream char)
    (declare (ignore char))
    `(ref ,(read stream t nil t))))

(eval-always
  (set-macro-character #\? #'var-reader)
  (set-macro-character #\! #'ref-reader))

(defmacro define-list-type (name elem)
  (let-gensym (x)
    `(progn
       (defun ,name (,x)
         (or (null ,x)
             (and (consp ,x)
                  (typep (car ,x) ',elem)
                  (,name (cdr ,x)))))
       (deftype ,name ()
         (list 'satisfies ',name)))))

(defparameter *current-test* '())

(defmacro deftest (name &body body)
  `(defun ,name ()
     (let ((*current-test* (cons ',name *current-test*)))
       ,@body
       (signal 'test-success :test (reverse *current-test*)))))

(define-condition test-success (condition)
  ((test :initarg :test
         :initform nil
         :reader test-name)))

(defun collect-test-results (func)
  (let (tests)
    (handler-bind
        ((test-success (lambda (condition)
                         (push (test-name condition) tests))))
      (funcall func)
      (reverse tests))))

(define-condition test-error (error)
  ((test :initarg :test
         :initform nil
         :reader test-name)
   (assertion :initarg :assertion
              :initform nil
              :reader test-assertion))
  (:report (lambda (condition stream)
             (format stream
                     "Test error in ~A: ~A"
                     (test-name condition)
                     (test-assertion condition)))))

(define-condition equal-test-error (test-error)
  ((test :initarg :test
         :initform nil
         :reader test-name)
   (assertion :initarg :assertion
              :initform nil
              :reader test-assertion)
   (operator :initarg :operator
             :initform nil
             :reader operator)
   (left :initarg :left
         :initform nil
         :reader left-hand-side)
   (right :initarg :right
          :initform nil
          :reader right-hand-side))
  (:report (lambda (condition stream)
             (format stream
                     "Equality test error in ~A:~%~A~%~A~%~A"
                     (test-name condition)
                     (left-hand-side condition)
                     (operator condition)
                     (right-hand-side condition)))))

(defmacro check (&rest tests)
  `(progn
     ,@(loop :for test :in tests
             :collect
             (match test
               ((list (and ?operator (member 'eq 'eql 'equal 'equalp '=)) ?left-form ?right-form)
                (let-gensym (left right)
                  `(let ((,left ,left-form)
                         (,right ,right-form))
                     (unless (,operator ,left ,right)
                       (error 'equal-test-error
                              :test (reverse *current-test*)
                              :assertion ',test
                              :operator ',operator
                              :left ,left
                              :right ,right)))))
               (t `(unless ,test
                     (error 'test-error :test (reverse *current-test*) :assertion ',test)))))))

(defmacro check-match (scrutinee &rest patterns)
  `(check (match ,scrutinee
                 ,@(loop :for pattern :in patterns
                         :collect `(,pattern t))
            (t nil))))
