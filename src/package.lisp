;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.raven
  (:use #:cl)
  (:export #:*fast-dispatch*)
  (:export #:route-metadata-error
           #:router-compilation-error
           #:no-path-generator-error
           #:no-route-function-error
           #:no-route-error)
  (:export #:route-metadata)
  (:export #:route-path)
  (:export #:compile-router
           #:define-route
           #:define-route-metadata)
  (:export #:%handle-request/fast
           #:%handle-request
           #:%make-route-metadata)
  (:documentation "URL dispatching package for Clack applications."))
