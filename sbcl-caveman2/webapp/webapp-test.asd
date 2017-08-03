(in-package :cl-user)
(defpackage webapp-test-asd
  (:use :cl :asdf))
(in-package :webapp-test-asd)

(defsystem webapp-test
  :author "AuthorName"
  :license ""
  :depends-on (:webapp
               :prove)
  :components ((:module "t"
                :components
                ((:file "webapp"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
