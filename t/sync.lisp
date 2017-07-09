(in-package :cl-user)
(defpackage sync-test
  (:use :cl
        :sync
        :prove))
(in-package :sync-test)

(cl-syntax:use-syntax :annot)

;; NOTE: To run this test file, execute `(asdf:test-system :sync)' in your Lisp.

(plan 4)

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


(defun genid-without-sync ()
  "generate id without synchronized"
  (sleep 1)
  (get-universal-time))

@synchronized-with
(defun genid-with-sync ()
  "generate id with synchronized"
  (sleep 1)
  (get-universal-time))

;; without synchronized
(let ((th-array (make-array 10))
      (result-array (make-array 10)))
  (loop for index from 0 below 10
       do (let ((x index))
            (setf (aref th-array x)
                  (bt:make-thread
                   (lambda ()
                     (setf (aref result-array x)
                           (genid-without-sync)))))))
  (loop for index from 0 below 10
        do (bt:join-thread (aref th-array index)))
  (let ((uniq-p T))
    (loop for index from 0 below 10
          do (when (not (= (position (aref result-array index) result-array) index))
               (format T "~A is not uniq~%" (aref result-array index))
               (setf uniq-p NIL)))
    (is uniq-p NIL
        "cannot generate unique id")))


;; with synchronized
(let ((th-array (make-array 10))
      (result-array (make-array 10)))
  (loop for index from 0 below 10
        do (let ((x index))
             (setf (aref th-array index)
                   (bt:make-thread
                    (lambda ()
                      (let ((*standard-output* #.*standard-output*))
                        (setf (aref result-array x)
                              (genid-with-sync))))))))
  (loop for index from 0 below 10
        do (bt:join-thread (aref th-array index)))
  (let ((uniq-p T))
    (loop for index from 0 below 10
          do (when (not (= (position (aref result-array index) result-array) index))
               (format T "~A is not uniq~%" (aref result-array index))
               (setf uniq-p NIL)))
    (is uniq-p T
        "can generate unique id")))

(finalize)
