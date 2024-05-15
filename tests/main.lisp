;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.raven/tests)

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

(deftest compile-router
    (testing
     "Compiles with static routes"
     (let ((router (foo.lisp.raven:compile-router
                    `(("/" ,'baaz)
                      ("/baar" ,'baar)
                      ("/hello" ,'hello)))))
       (ok (functionp router))))

  (testing
   "Compiles with a dynamic route"
   (let ((router (foo.lisp.raven:compile-router
                  `(("/:foo-id" ,'foo)))))
     (ok (functionp router))))

  (testing
   "Compiles with a dynamic route with multiple bindings"
   (let ((router (foo.lisp.raven:compile-router
                  `(("/:foo-id/hello/:bar" ,'foo)))))
     (ok (functionp router))))

  (testing
   "Compiles with a dynamic route on the same level as static routes"
   (let ((router (foo.lisp.raven:compile-router
                  `(("/baaz" ,'baaz)
                    ("/:foo-id" ,'foo)
                    ("/baar" ,'baar)))))
     (ok (functionp router))))

  (testing
   "Signals a simple-error when a path is an empty string"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("" ,'baaz)))
                'simple-error)))

  (testing
   "Signals a simple-error when a path is an empty binding"
   (ok (signals (foo.lisp.raven:compile-router
                 `((":" ,'baaz)))
                'simple-error)))

  (testing
   "Signals a simple-error when a path is an empty binding at a depth greater than zero"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo/:" ,'baaz)))
                'simple-error)))

  (testing
   "Signals a simple-error when zero routes are given"
   (ok (signals (foo.lisp.raven:compile-router
                 `())
                'simple-error)))

  (testing
   "Signals a simple-error when duplicate static routes are given"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/hello" ,'baaz)
                   ("/hello" ,'hello)
                   ("/baar" ,'baar)))
                'simple-error))

   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo/baaz" ,'baaz)
                   ("/foo/baaz" ,'hello)
                   ("/bar/quux" ,'quux)
                   ("/foo/baar" ,'baar)))
                'simple-error)))

  (testing
   "Signals a simple-error when a route has an multiple forward slashes in a sequence"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("//" ,'baaz)))
                'simple-error))

   (ok (signals (foo.lisp.raven:compile-router
                 `(("//quux" ,'quux)))
                'simple-error))

   (ok (signals (foo.lisp.raven:compile-router
                 `(("/quux//" ,'quux)))
                'simple-error)))

  (testing
   "Signals a simple-error when there are multiple dynamic routes terminating at the same level"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/:bar" ,'baaz)
                   ("/:baaz" ,'hello)
                   ("/bar/quux" ,'quux)
                   ("/baar" ,'baar)))
                'simple-error))

   (ok (signals (foo.lisp.raven:compile-router
                 `(("/baaz/:bar" ,'quux)
                   ("/baaz/:baaz" ,'hello)))
                'simple-error)))

  (testing
   "Signals a simple-error when a path contains duplicate bindings"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo/:bar/hello/:bar" ,'quux)))
                'simple-error)))

  (testing
   "Signals a simple-error when multiple routes share a dynamic segment but are named differently"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo/quux/:bar/baaz" ,'quux)
                   ("/foo/quux/:foo" ,'baar)))
                'simple-error))

   (ok (signals (foo.lisp.raven:compile-router
                 `(("/quux/:bar/baaz" ,'quux)
                   ("/quux/:foo/bar" ,'baar)))
                'simple-error))

   (ok (foo.lisp.raven:compile-router
        `(("/quux/:bar/baaz" ,'quux)
          ("/baar/:foo/baaz" ,'baar)))))

  (testing
   "Signals a simple-error when a handler symbol does not have a function value"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/" ,'bogus)))
                'simple-error)))

  (testing
   "Signals a simple-error when a handler symbol corresponds to more than one route"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo" ,'quux)
                   ("/bar" ,'quux)))
                'simple-error)))

  (testing
   "Signals a simple-error when a binding occurs more than once"
   (ok (signals (foo.lisp.raven:compile-router
                 `(("/foo/:foo/bar/:foo" ,'quux)))
                'simple-error))))

(deftest dispatch
    (testing
     "Dispatches to static routes"
     (let* ((router (foo.lisp.raven:compile-router
                     `(("/" ,'baaz)
                       ("/baar" ,'baar)
                       ("/hello" ,'hello))))
            (app (funcall router :clack)))
       (ok (functionp app))
       (let ((response (funcall app '(:path-info "/"))))
         (ok response)
         (ok (equalp response `(200 () ("Baaz")))))
       (let ((response (funcall app '(:path-info "/hello"))))
         (ok response)
         (ok (equalp response `(200 () ("Hello")))))
       (let ((response (funcall app '(:path-info "/baar"))))
         (ok response)
         (ok (equalp response `(200 () ("Baar")))))))

  (testing
   "Returns a default 404 text/plain response when there is no matching route"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/" ,'baaz)
                     ("/baar" ,'baar)
                     ("/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/bogus"))))
       (ok response)
       (ok (equalp response `(404 (:content-type "text/plain"
                                   :content-length 9)
                                  ("Not Found")))))))

  (testing
   "Dispatches to a dynamic route"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/:foo-id" ,'foo))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/foobar"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: foobar")))))))

  (testing
   "Dispatches to a dynamic route on the same level as static routes"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/" ,'baaz)
                     ("/baar" ,'baar)
                     ("/:foo-id" ,'foo)
                     ("/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/quux"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: quux")))))))

  (testing
   "Dispatches to a dynamic route after matching against static routes"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/" ,'baaz)
                     ("/:foo-id" ,'foo)
                     ("/baar" ,'baar)
                     ("/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/hello"))))
       (ok response)
       (ok (equalp response `(200 () ("Hello")))))
     (let ((response (funcall app '(:path-info "/"))))
       (ok response)
       (ok (equalp response `(200 () ("Baaz")))))
     (let ((response (funcall app '(:path-info "/baar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baar")))))
     (let ((response (funcall app '(:path-info "/baaz"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: baaz")))))))

  (testing
   "Dispatches to a dynamic route after matching against static routes when there is more than one path component"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/baaz/bar" ,'baaz)
                     ("/baaz/bar/:foo-id" ,'foo)
                     ("/baaz/bar/baar" ,'baar)
                     ("/baaz/bar/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/baaz/bar/hello"))))
       (ok response)
       (ok (equalp response `(200 () ("Hello")))))
     (let ((response (funcall app '(:path-info "/baaz/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baaz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baar")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baaz"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: baaz")))))))

  (testing
   "Dispatches to a dynamic route when there are multiple dynamic routes at the same path level but they do not terminate at the same level"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/baaz/bar" ,'baaz)
                     ("/baaz/bar/:foo-id/baaz" ,'foo)
                     ("/baaz/bar/:foo-id/foo" ,'foobar)
                     ("/baaz/bar/baar" ,'baar)
                     ("/baaz/bar/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/baaz/bar/abc/baaz"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: abc")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/xyz/baaz"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: xyz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/abc/foo"))))
       (ok response)
       (ok (equalp response `(200 () ("Foobar: abc")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/xyz/foo"))))
       (ok response)
       (ok (equalp response `(200 () ("Foobar: xyz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baaz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baar")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/hello"))))
       (ok response)
       (ok (equalp response `(200 () ("Hello")))))))

  (testing
   "Dispatches to route with multiple sequential dynamic components"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/:foo/:bar/:baaz" ,'baaz))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/foo/quux/baar"))))
       (ok response)
       (ok (equal response `(200 () ("Baaz")))))))

  (testing
   "Dispatches to routes with trailing slashes"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/baaz/bar" ,'baaz)
                     ("/baaz/bar/:foo-id" ,'foo)
                     ("/baaz/bar/baar" ,'baar)
                     ("/baaz/bar/baar/" ,'foobaaz)
                     ("/baaz/bar/" ,'quux)
                     ("/baaz/bar/hello" ,'hello))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/baaz/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baaz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/"))))
       (ok response)
       (ok (equalp response `(200 () ("Quux")))))
     (let ((response (funcall app '(:path-info "/baaz/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baaz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baar"))))
       (ok response)
       (ok (equalp response `(200 () ("Baar")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baar/"))))
       (ok response)
       (ok (equalp response `(200 () ("Foobaaz")))))
     (let ((response (funcall app '(:path-info "/baaz/bar/baaz"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: baaz")))))))

  (testing
   "Does not dispatch a dynamic route to a trailing slash route (no binding as empty string)"
   (let* ((router (foo.lisp.raven:compile-router
                   `(("/:foo-id" ,'foo))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "//"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: bar"))))))

   (let* ((router (foo.lisp.raven:compile-router
                   `(("/foo/:foo-id" ,'foo))))
          (app (funcall router :clack)))
     (let ((response (funcall app '(:path-info "/bar"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/bar/"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/foo"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/foo/"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/foo//"))))
       (ok response)
       (ok (= 404 (first response))))
     (let ((response (funcall app '(:path-info "/foo/bar"))))
       (ok response)
       (ok (equalp response `(200 () ("Foo: bar"))))))))

(deftest encodings
    (testing
     "Compiles and dispatches with Unicode and reserved characters"
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
         (ok response)
         (ok (equalp response `(200 () ("Hello")))))
       (let ((response (funcall app '(:path-info "/你好"))))
         (ok response)
         (ok (equalp response `(200 () ("Baaz")))))
       (let ((response (funcall app '(:path-info "/🚀"))))
         (ok response)
         (ok (equalp response `(200 () ("Baar")))))
       (let ((response (funcall app '(:path-info "/🔥"))))
         (ok response)
         (ok (equalp response `(200 () ("🔥")))))
       (let ((response (funcall app '(:path-info "/αθήνα/ελληνικά"))))
         (ok response)
         ;; Decodes binding value
         (ok (equalp response `(200 () ("Foobar: αθήνα")))))
       (let ((response (funcall app '(:path-info "/hello world"))))
         (ok response)
         (ok (equalp response `(200 () ("Quux")))))
       (let ((response (funcall app '(:path-info "/apples&oranges"))))
         (ok response)
         (ok (equalp response `(200 () ("Bar"))))))))

(deftest find-route
    (testing
     "Returns a route-info struct for the specified path or returns NIL when the path has no route"
     (let ((router (foo.lisp.raven:compile-router
                    `(("/foo" ,'foo)
                      ("/:baaz" ,'baaz)
                      ("/:baaz/bar" ,'bar)
                      ("/:baaz/foo/:quux/hello" ,'quux)))))
       (ok (foo.lisp.raven::route-info-p (funcall router '(:find-route "/foo"))))
       (ok (foo.lisp.raven::route-info-p (funcall router '(:find-route "/hello"))))
       (ok (null (foo.lisp.raven::route-info-p (funcall router '(:find-route "/")))))
       (ok (null (foo.lisp.raven::route-info-p (funcall router '(:find-route "/hello/")))))
       (ok (foo.lisp.raven::route-info-p (funcall router '(:find-route "/hello/bar"))))
       (ok (null (foo.lisp.raven::route-info-p (funcall router '(:find-route "/hello/bar/")))))
       (ok (null (foo.lisp.raven::route-info-p (funcall router '(:find-route "/hello/world")))))
       (ok (equal "/foo" (foo.lisp.raven::route-info-path-spec
                          (funcall router '(:find-route "/foo")))))
       (ok (equal "/foo" (foo.lisp.raven::route-info-path
                          (funcall router '(:find-route "/foo")))))
       (ok (equal "/:baaz" (foo.lisp.raven::route-info-path-spec
                            (funcall router '(:find-route "/hello")))))
       (ok (equalp '((:|baaz| . "hello")) (foo.lisp.raven::route-info-bindings
                                           (funcall router '(:find-route "/hello")))))
       (ok (equal "/hello" (foo.lisp.raven::route-info-path
                            (funcall router '(:find-route "/hello")))))
       (ok (equal "/:baaz/bar" (foo.lisp.raven::route-info-path-spec
                                (funcall router '(:find-route "/hello/bar")))))
       (ok (equalp '((:|baaz| . "hello")) (foo.lisp.raven::route-info-bindings
                                           (funcall router '(:find-route "/hello/bar")))))
       (ok (equal "/hello/bar" (foo.lisp.raven::route-info-path
                                (funcall router '(:find-route "/hello/bar")))))
       (ok (equal "/hello/foo/foobar/hello" (foo.lisp.raven::route-info-path
                                             (funcall router '(:find-route "/hello/foo/foobar/hello")))))
       (ok (equal "/:baaz/foo/:quux/hello" (foo.lisp.raven::route-info-path-spec
                                            (funcall router '(:find-route "/hello/foo/foobar/hello")))))
       (ok (equalp '((:|baaz| . "hello") (:|quux| . "foobar"))
                   (foo.lisp.raven::route-info-bindings
                    (funcall router '(:find-route "/hello/foo/foobar/hello"))))))))

