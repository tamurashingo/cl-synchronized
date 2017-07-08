(in-package :cl-user)
(defpackage cl-synchronized
  (:use :cl :asdf))
(in-package :cl-synchronized)

(defsystem cl-synchronized
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:sync))
