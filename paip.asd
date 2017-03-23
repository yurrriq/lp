;;;; paip.asd

(asdf:defsystem paip
  :description "Paradigms of Artificial Intelligence Programming exercises"
  :author "Eric Bailey <eric@ericb.me>"
  ;; TODO :license "Specify license here"
  :depends-on (:lisp-unit)
  :serial t
  :components ((:module "src"
                :components
                ((:module "paip"
                  :components
                  ((:file "intro")
                   (:file "gps")))))))

(defpackage paip
  (:use :cl))
(in-package :paip)
