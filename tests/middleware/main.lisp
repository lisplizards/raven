;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/raven/tests/main)

(defun gizmos (env)
  (declare (ignore env))
  `(200
    (:content-type "text/plain")
    ("Gizmos")))

(defun gizmo (env)
  (declare (ignore env))
  `(200
    (:content-type "text/plain")
    ("Gizmo")))

(deftest routes ()
  (testing
   "Dispatches to the matching handler"
   (flet ((app (env)
            (declare (ignore env))
            `(404
              (:content-type "text/plain"
               :content-length 9)
              ("Not Found"))))
     (let* ((app (funcall lack/middleware/raven:*lack-middleware-raven*
                          #'app
                          :routes `(("/gizmos" ,'gizmos)
                                    ("/gizmos/:gizmo-id" ,'gizmo))))
            (gizmos-response (funcall app '(:path-info "/gizmos" :request-method :GET)))
            (gizmo-response (funcall app '(:path-info "/gizmos/xyz" :request-method :GET))))
       (ok (= 200 (first gizmos-response)))
       (ok (string= "Gizmos" (first (third gizmos-response))))
       (ok (= 200 (first gizmo-response)))
       (ok (string= "Gizmo" (first (third gizmo-response))))
       (ok (signals (funcall app '(:path-info "/foobar" :request-method :GET))
                    'foo.lisp.raven:no-route-error))))))
