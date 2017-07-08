(in-package :cl-user)
(defpackage sync-test
  (:use :cl
        :sync
        :prove))
(in-package :sync-test)

;; NOTE: To run this test file, execute `(asdf:test-system :sync)' in your Lisp.

(plan 2)

(defparameter *SYNCHRONIZED* (make-synchronized))

;; without synchronized
(defparameter *val* 0)
(let ((th-array (make-array 10)))
  (loop for index from 0 below 10
        do (setf (aref th-array index)
                 (bt:make-thread
                   (lambda ()
                     (let ((v *val*)
                           (*standard-output* #.*standard-output*))
                       (format T "before:~A~%" *val*)
                       (sleep (random 0.5))
                       (setf *val* (1+ v))
                       (format T "after :~A~%" *val*))))))
  (loop for index from 0 below 10
        do (bt:join-thread (aref th-array index))))

(isnt *val* 10
      "non synchronized")


;; with synchronized
(defparameter *val* 0)
(let ((th-array (make-array 10)))
  (loop for index from 0 below 10
        do (setf (aref th-array index)
                 (bt:make-thread
                   (lambda ()
                     (synchronized *SYNCHRONIZED*
                       (let ((v *val*)
                             (*standard-output* #.*standard-output*))
                         (format T "before:~A~%" *val*)
                         (sleep (random 0.5))
                         (setf *val* (1+ v))
                         (format T "after :~A~%" *val*)))))))
  (loop for index from 0 below 10
        do (bt:join-thread (aref th-array index))))

(is *val* 10
    "synchronized")

(finalize)
