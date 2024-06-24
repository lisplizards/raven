;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-raven"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/raven"
  :bug-tracker "https://github.com/lisplizards/raven/issues"
  :source-control (:git "https://github.com/lisplizards/raven.git")
  :depends-on ("foo.lisp.raven")
  :components ((:module "src"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package"))
                   (:file "package"))))))
  :description "URL dispatcher Lack middleware"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-raven/tests"))))

(defsystem "foo.lisp.lack-middleware-raven/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-raven"
               "rove")
  :components ((:module "tests"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package"))
                   (:file "package"))))))
  :description "Test system for foo.lisp.lack-middleware-raven"
  :perform (test-op (op c) (symbol-call :rove :run c)))
