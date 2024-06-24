;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/raven)

(defparameter *lack-middleware-raven*
  (lambda (app &key routes fast-dispatch)
    (declare (type function app)
             (type list routes)
             (type boolean fast-dispatch))
    (check-type routes list)
    (check-type fast-dispatch boolean)
    (let* ((router (foo.lisp.raven:compile-router
                    routes
                    :fast-dispatch fast-dispatch)))
      (declare (type function router))
      (the function (funcall router :clack)))))
