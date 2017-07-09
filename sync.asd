#|
  This file is a part of CL-Synchronized project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

#|
  mutex operation (like Java's synchronized) for CommonLisp

  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage sync-asd
  (:use :cl :asdf))
(in-package :sync-asd)

(defsystem sync
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:bordeaux-threads
               :cl-syntax
               :cl-syntax-annot
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "sync"))))
  :description "mutex operation (like Java's synchronized) for CommonLisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sync-test))))
