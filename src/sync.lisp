(in-package :cl-user)
(defpackage sync
  (:use :cl)
  (:export :make-synchronized
           :synchronized))
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

