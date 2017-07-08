#|
  This file is a part of CL-Synchronized project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage sync-test-asd
  (:use :cl :asdf))
(in-package :sync-test-asd)

(defsystem sync-test
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:sync
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "sync"))))
  :description "Test system for CL-Synchronized"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
