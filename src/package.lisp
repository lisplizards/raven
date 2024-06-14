;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.raven
  (:use #:cl)
  (:export #:*fast-dispatch*
           #:compile-router
           #:define-route
           #:define-route-metadata
           #:route-metadata
           #:invalid-route-error
           #:no-route-error
           #:%handle-request/fast
           #:%handle-request
           #:%make-route-metadata))
