;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.raven"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/raven"
  :bug-tracker "https://github.com/lisplizards/raven/issues"
  :source-control (:git "https://github.com/lisplizards/raven.git")
  :depends-on ("fast-generic-functions"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package" "types"))
                 (:file "types" :depends-on ("package"))
                 (:file "package"))))
  :description "Trie-based HTTP router"
  :in-order-to ((test-op (test-op "foo.lisp.raven/tests"))))

(defsystem "foo.lisp.raven/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.raven"
               "parachute")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.raven"
  :perform (test-op (op c) (symbol-call :parachute :test :foo.lisp.raven/tests/main)))
