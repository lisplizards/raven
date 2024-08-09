;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.raven/tests/other)

(defun baaz (env)
  (declare (ignore env))
  `(200 () ("Baaz!")))

(defun quuxfoo (env)
  (declare (ignore env))
  `(200 () ("Quuxfoo")))

(in-package #:foo.lisp.raven/tests/main)

(defun baaz (env)
  (declare (ignore env))
  `(200 () ("Baaz")))

(defun baar (env)
  (declare (ignore env))
  `(200 () ("Baar")))

(defun bar (env)
  (declare (ignore env))
  `(200 () ("Bar")))

(defun hello (env)
  (declare (ignore env))
  `(200 () ("Hello")))

(defun foo (env)
  "The foo route"
  (let* ((bindings (getf env :raven.binding))
         (body (format nil "Foo: ~A" (cdr (assoc :|foo-id| bindings)))))
    `(200 () (,body))))

(defun foobar (env)
  (let* ((bindings (getf env :raven.binding))
         (body (format nil "Foobar: ~A" (cdr (assoc :|foo-id| bindings)))))
    `(200 () (,body))))

(defun foobaaz (env)
  (declare (ignore env))
  `(200 () ("Foobaaz")))

(defun quux (env)
  "The quux route"
  (declare (ignore env))
  `(200 () ("Quux")))

(defun 🔥 (env)
  (declare (ignore env))
  `(200 () ("🔥")))

(define-test router)

(define-test compile-router
  :parent router
  (define-test
      "Compiles with static routes"
      (let ((router (foo.lisp.raven:compile-router
                     `(("/" ,'baaz)
                       ("/baar" ,'baar)
                       ("/hello" ,'hello)))))
        (true (functionp router))))

  (define-test
      "Compiles with a dynamic route"
      (let ((router (foo.lisp.raven:compile-router
                     `(("/:foo-id" ,'foo)))))
        (true (functionp router))))

  (define-test
      "Compiles with a dynamic route with multiple bindings"
      (let ((router (foo.lisp.raven:compile-router
                     `(("/:foo-id/hello/:bar" ,'foo)))))
        (true (functionp router))))

  (define-test
      "Compiles with a dynamic route on the same level as static routes"
      (let ((router (foo.lisp.raven:compile-router
                     `(("/baaz" ,'baaz)
                       ("/:foo-id" ,'foo)
                       ("/baar" ,'baar)))))
        (true (functionp router))))

  (define-test
      "Signals a simple-error when a path is an empty string"
      (fail (foo.lisp.raven:compile-router
             `(("" ,'baaz)))
            'simple-error))

  (define-test
      "Signals a simple-error when a path is an empty binding"
      (fail (foo.lisp.raven:compile-router
             `((":" ,'baaz)))
            'simple-error))

  (define-test
      "Signals a simple-error when a path is an empty binding at a depth greater than zero"
      (fail (foo.lisp.raven:compile-router
             `(("/foo/:" ,'baaz)))
            'simple-error))

  (define-test
      "Signals a simple-error when zero routes are given"
      (fail (foo.lisp.raven:compile-router
             `())
            'simple-error))

  (define-test
      "Signals a simple-error when duplicate static routes are given"
      (fail (foo.lisp.raven:compile-router
             `(("/hello" ,'baaz)
               ("/hello" ,'hello)
               ("/baar" ,'baar)))
            'simple-error)

    (fail (foo.lisp.raven:compile-router
           `(("/foo/baaz" ,'baaz)
             ("/foo/baaz" ,'hello)
             ("/bar/quux" ,'quux)
             ("/foo/baar" ,'baar)))
          'simple-error))

  (define-test
      "Signals a simple-error when a route has an multiple forward slashes in a sequence"
      (fail (foo.lisp.raven:compile-router
             `(("//" ,'baaz)))
            'simple-error)

    (fail (foo.lisp.raven:compile-router
           `(("//quux" ,'quux)))
          'simple-error)

    (fail (foo.lisp.raven:compile-router
           `(("/quux//" ,'quux)))
          'simple-error))

  (define-test
      "Signals a simple-error when there are multiple dynamic routes terminating at the same level"
      (fail (foo.lisp.raven:compile-router
             `(("/:bar" ,'baaz)
               ("/:baaz" ,'hello)
               ("/bar/quux" ,'quux)
               ("/baar" ,'baar)))
            'simple-error)

    (fail (foo.lisp.raven:compile-router
           `(("/baaz/:bar" ,'quux)
             ("/baaz/:baaz" ,'hello)))
          'simple-error))

  (define-test
      "Signals a simple-error when a path contains duplicate bindings"
      (fail (foo.lisp.raven:compile-router
             `(("/foo/:bar/hello/:bar" ,'quux)))
            'simple-error))

  (define-test
      "Signals a simple-error when multiple routes share a dynamic segment but are named differently"
      (fail (foo.lisp.raven:compile-router
             `(("/foo/quux/:bar/baaz" ,'quux)
               ("/foo/quux/:foo" ,'baar)))
            'simple-error)

    (fail (foo.lisp.raven:compile-router
           `(("/quux/:bar/baaz" ,'quux)
             ("/quux/:foo/bar" ,'baar)))
          'simple-error)

    (true (foo.lisp.raven:compile-router
           `(("/quux/:bar/baaz" ,'quux)
             ("/baar/:foo/baaz" ,'baar)))))

  (define-test
      "Signals a simple-error when a handler symbol does not have a function value"
      (fail (foo.lisp.raven:compile-router
             `(("/" ,'bogus)))
            'foo.lisp.raven:no-route-function-error))

  (define-test
      "Signals a simple-error when a handler symbol corresponds to more than one route"
      (fail (foo.lisp.raven:compile-router
             `(("/foo" ,'quux)
               ("/bar" ,'quux)))
            'simple-error))

  (define-test
      "Signals a simple-error when there are symbols of the same name belonging to separate packages"
      (fail (foo.lisp.raven:compile-router
             `(("/" ,'baaz)
               ("/baar" ,'foo.lisp.raven/tests/other::baaz)))
            'simple-error))

  (define-test
      "Allows specifying a route from another package (if it does not have the same symbol name)"
      (true (foo.lisp.raven:compile-router
             `(("/" ,'foo)
               ("/baar" ,'foo.lisp.raven/tests/other::baaz)))))

  (define-test
      "Signals a simple-error when a binding occurs more than once"
      (fail (foo.lisp.raven:compile-router
             `(("/foo/:foo/bar/:foo" ,'quux)))
            'simple-error)))

(define-test dispatch
  :parent router
  (define-test "Dispatches to static routes"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/" ,'baaz)
                        ("/baar" ,'baar)
                        ("/hello" ,'hello))))
             (app (funcall router :clack)))
        (true (functionp app))
        (let ((response (funcall app '(:path-info "/"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/hello"))))
          (true response)
          (true (equalp response `(200 () ("Hello")))))
        (let ((response (funcall app '(:path-info "/baar"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))))

  (define-test "Signals NO-ROUTE-ERROR there is no matching route"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/" ,'baaz)
                        ("/baar" ,'baar)
                        ("/hello" ,'hello))))
             (app (funcall router :clack)))
        (fail (funcall app '(:path-info "/bogus"))
              'foo.lisp.raven:no-route-error)))

  (define-test "Dispatches to a dynamic route"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/:foo-id" ,'foo))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/foobar"))))
          (true response)
          (true (equalp response `(200 () ("Foo: foobar")))))))

  (define-test "Dispatches to a dynamic route on the same level as static routes"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/" ,'baaz)
                        ("/baar" ,'baar)
                        ("/:foo-id" ,'foo)
                        ("/hello" ,'hello))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/quux"))))
          (true response)
          (true (equalp response `(200 () ("Foo: quux")))))))

  (define-test "Dispatches to a dynamic route after matching against static routes"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/" ,'baaz)
                        ("/:foo-id" ,'foo)
                        ("/baar" ,'baar)
                        ("/hello" ,'hello))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/hello"))))
          (true response)
          (true (equalp response `(200 () ("Hello")))))
        (let ((response (funcall app '(:path-info "/"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/baar"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))
        (let ((response (funcall app '(:path-info "/baaz"))))
          (true response)
          (true (equalp response `(200 () ("Foo: baaz")))))))

  (define-test "Dispatches to a dynamic route after matching against static routes when there is more than one path component"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/baaz/bar" ,'baaz)
                        ("/baaz/bar/:foo-id" ,'foo)
                        ("/baaz/bar/baar" ,'baar)
                        ("/baaz/bar/hello" ,'hello))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/baaz/bar/hello"))))
          (true response)
          (true (equalp response `(200 () ("Hello")))))
        (let ((response (funcall app '(:path-info "/baaz/bar"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baaz"))))
          (true response)
          (true (equalp response `(200 () ("Foo: baaz")))))))

  (define-test "Dispatches to a dynamic route when there are multiple dynamic routes at the same path level but they do not terminate at the same level"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/baaz/bar" ,'baaz)
                        ("/baaz/bar/:foo-id/baaz" ,'foo)
                        ("/baaz/bar/:foo-id/foo" ,'foobar)
                        ("/baaz/bar/baar" ,'baar)
                        ("/baaz/bar/hello" ,'hello))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/baaz/bar/abc/baaz"))))
          (true response)
          (true (equalp response `(200 () ("Foo: abc")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/xyz/baaz"))))
          (true response)
          (true (equalp response `(200 () ("Foo: xyz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/abc/foo"))))
          (true response)
          (true (equalp response `(200 () ("Foobar: abc")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/xyz/foo"))))
          (true response)
          (true (equalp response `(200 () ("Foobar: xyz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/hello"))))
          (true response)
          (true (equalp response `(200 () ("Hello")))))))

  (define-test "Dispatches to route with multiple sequential dynamic components"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/:foo/:bar/:baaz" ,'baaz))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/foo/quux/baar"))))
          (true response)
          (true (equal response `(200 () ("Baaz")))))))

  (define-test "Dispatches to routes with trailing slashes"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/baaz/bar" ,'baaz)
                        ("/baaz/bar/:foo-id" ,'foo)
                        ("/baaz/bar/baar" ,'baar)
                        ("/baaz/bar/baar/" ,'foobaaz)
                        ("/baaz/bar/" ,'quux)
                        ("/baaz/bar/hello" ,'hello))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/baaz/bar"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/"))))
          (true response)
          (true (equalp response `(200 () ("Quux")))))
        (let ((response (funcall app '(:path-info "/baaz/bar"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baar/"))))
          (true response)
          (true (equalp response `(200 () ("Foobaaz")))))
        (let ((response (funcall app '(:path-info "/baaz/bar/baaz"))))
          (true response)
          (true (equalp response `(200 () ("Foo: baaz")))))))

  (define-test "Does not dispatch a dynamic route to a trailing slash route (no binding as empty string)"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/:foo-id" ,'foo))))
             (app (funcall router :clack)))
        (fail (funcall app '(:path-info "/"))
              'foo.lisp.raven:no-route-error)
        (fail (funcall app '(:path-info "//"))
              'foo.lisp.raven:no-route-error)
        (let ((response (funcall app '(:path-info "/bar"))))
          (true response)
          (true (equalp response `(200 () ("Foo: bar"))))))

    (let* ((router (foo.lisp.raven:compile-router
                    `(("/foo/:foo-id" ,'foo))))
           (app (funcall router :clack)))
      (fail (funcall app '(:path-info "/bar"))
            'foo.lisp.raven:no-route-error)
      (fail (funcall app '(:path-info "/bar/"))
            'foo.lisp.raven:no-route-error)
      (fail (funcall app '(:path-info "/foo"))
            'foo.lisp.raven:no-route-error)
      (fail (funcall app '(:path-info "/foo/"))
            'foo.lisp.raven:no-route-error)
      (fail (funcall app '(:path-info "/foo//"))
            'foo.lisp.raven:no-route-error)
      (let ((response (funcall app '(:path-info "/foo/bar"))))
        (true response)
        (true (equalp response `(200 () ("Foo: bar"))))))))

(define-test encodings
  :parent router
  (define-test "Compiles and dispatches with Unicode and reserved characters"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/привет" ,'hello)
                        ("/你好" ,'baaz)
                        ("/🚀" ,'baar)
                        ("/🔥" ,'🔥)
                        ("/:foo-id/ελληνικά" ,'foobar)
                        ("/hello world" ,'quux)
                        ("/apples&oranges" ,'bar))))
             (app (funcall router :clack)))
        (let ((response (funcall app '(:path-info "/привет"))))
          (true response)
          (true (equalp response `(200 () ("Hello")))))
        (let ((response (funcall app '(:path-info "/你好"))))
          (true response)
          (true (equalp response `(200 () ("Baaz")))))
        (let ((response (funcall app '(:path-info "/🚀"))))
          (true response)
          (true (equalp response `(200 () ("Baar")))))
        (let ((response (funcall app '(:path-info "/🔥"))))
          (true response)
          (true (equalp response `(200 () ("🔥")))))
        (let ((response (funcall app '(:path-info "/αθήνα/ελληνικά"))))
          (true response)
          ;; Decodes binding value
          (true (equalp response `(200 () ("Foobar: αθήνα")))))
        (let ((response (funcall app '(:path-info "/hello world"))))
          (true response)
          (true (equalp response `(200 () ("Quux")))))
        (let ((response (funcall app '(:path-info "/apples&oranges"))))
          (true response)
          (true (equalp response `(200 () ("Bar"))))))))

(define-test find-route
  :parent router
  (define-test
      "Returns a find-route-result struct for the specified path or returns NIL when the path has no route"
      (let ((router (foo.lisp.raven:compile-router
                     `(("/foo" ,'foo)
                       ("/:baaz" ,'baaz)
                       ("/:baaz/bar" ,'bar)
                       ("/:baaz/foo/:quux/hello" ,'quux)))))
        (true (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/foo"))))
        (true (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/hello"))))
        (true (null (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/")))))
        (true (null (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/hello/")))))
        (true (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/hello/bar"))))
        (true (null (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/hello/bar/")))))
        (true (null (foo.lisp.raven::find-route-result-p (funcall router '(:find-route "/hello/world")))))
        (let ((result (funcall router '(:find-route "/foo"))))
          (true (equal "/foo" (foo.lisp.raven::route-info-path-spec
                               (foo.lisp.raven::find-route-result-route-info
                                result)))))
        (true (equal "/foo" (foo.lisp.raven::find-route-result-path
                             (funcall router '(:find-route "/foo")))))
        (let ((result (funcall router '(:find-route "/hello"))))
          (true (equal "/:baaz" (foo.lisp.raven::route-info-path-spec
                                 (foo.lisp.raven::find-route-result-route-info
                                  result)))))
        (true (equalp '((:|baaz| . "hello"))
                      (foo.lisp.raven::find-route-result-bindings
                       (funcall router '(:find-route "/hello")))))
        (true (equal "/hello" (foo.lisp.raven::find-route-result-path
                               (funcall router '(:find-route "/hello")))))
        (let ((result (funcall router '(:find-route "/hello/bar"))))
          (true (equal "/:baaz/bar" (foo.lisp.raven::route-info-path-spec
                                     (foo.lisp.raven::find-route-result-route-info
                                      result)))))
        (true (equalp '((:|baaz| . "hello"))
                      (foo.lisp.raven::find-route-result-bindings
                       (funcall router '(:find-route "/hello/bar")))))
        (true (equal "/hello/bar" (foo.lisp.raven::find-route-result-path
                                   (funcall router '(:find-route "/hello/bar")))))
        (true (equal "/hello/foo/foobar/hello" (foo.lisp.raven::find-route-result-path
                                                (funcall router '(:find-route "/hello/foo/foobar/hello")))))
        (let ((result (funcall router '(:find-route "/hello/foo/foobar/hello"))))
          (true (equal "/:baaz/foo/:quux/hello"
                       (foo.lisp.raven::route-info-path-spec
                        (foo.lisp.raven::find-route-result-route-info
                         result)))))
        (true (equalp '((:|baaz| . "hello") (:|quux| . "foobar"))
                      (foo.lisp.raven::find-route-result-bindings
                       (funcall router '(:find-route "/hello/foo/foobar/hello"))))))))
