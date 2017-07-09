(in-package :cl-user)
(defpackage sync
  (:use :cl
        :cl-annot)
  (:export :make-synchronized
           :synchronized
           :synchronized-with))
(in-package :sync)

(defclass <synchronized> ()
  ((lock :accessor lock
         :initform (bt:make-lock))))

(defun make-synchronized ()
  "create a mutex object"
  (make-instance '<synchronized>))

(defmacro synchronized (sync-obj &body body)
  "synchronized operator like Java"
  (let ((lock (gensym)))
    `(progn
       (let ((,lock (lock ,sync-obj)))
         (bt:acquire-lock ,lock T)
         (unwind-protect
              (progn
                ,@body)
           (bt:release-lock ,lock))))))

(defannotation synchronized-with (definition-form)
    (:arity 1)
  "synchronized annotation for DEFINITION-FORM"
  (let ((sync-object (gensym))
        (def-name (cadr definition-form))
        (def-args (caddr definition-form))
        (def-body (cdddr definition-form)))
    `(progn
       (defvar ,sync-object (make-synchronized))
       (defun ,def-name ,def-args
         (synchronized ,sync-object
           ,@def-body)))))

