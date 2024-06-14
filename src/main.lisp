;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.raven)

(defvar *fast-dispatch* nil)

(define-condition invalid-route-error (error)
  ((route-name :initarg :route-name
               :type symbol)
   (options :initarg :options
            :type list)
   (original-error :initarg :original-error))
  (:report (lambda (condition stream)
             (with-slots (route-name options original-error) condition
               (format stream "Invalid route: ~A~%Options: ~A~%Original error: ~A"
                       route-name
                       options
                       original-error))))
  (:documentation "Error condition signalled when the route definition is invalid."))

(define-condition no-route-error (simple-error)
  ((path-info :initarg :path-info
              :type (simple-array character (*))))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No route handler for request")))
  (:documentation "Error signalled when the route cannot be matched."))

(defstruct node
  (binding nil :type (or null keyword))
  (route-name nil :type symbol)
  (route-uri-template nil :type (or null string))
  (route-metadata nil)
  (children nil :type (or null hash-table)))

(defstruct (route-info
            (:print-function
             (lambda (struct stream depth)
               (declare (ignore depth))
               (format stream "Route~%-----~%Name: ~A~%Pattern: ~A~%Path: ~A~%Bindings: ~A~%Metadata: ~A~%Documentation: ~A~%"
                       (node-route-name (route-info-node struct))
                       (route-info-path-spec struct)
                       (route-info-path struct)
                       (route-info-bindings struct)
                       (node-route-metadata (route-info-node struct))
                       (documentation (node-route-name (route-info-node struct)) 'function)))))
  path-spec
  path
  bindings
  node)

(declaim (inline find-node)
         (ftype (function (node string) (values (or null node) list string-list)) find-node))
(defun find-node (root path)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type node root)
           (type string path))
  (let ((current-node root)
        (bindings ())
        (path-spec-components ())
        (split-path (if (equalp path "/")
                        '("")
                        (cdr (uiop:split-string path :separator "/")))))
    (declare (type node current-node)
             (type list bindings path-spec-components)
             (type string-list split-path))
    (dolist (path-component split-path)
      (declare (type string path-component)
               (type string-list split-path))
      (let* ((node-children (node-children current-node))
             (node (and node-children (gethash path-component node-children))))
        (declare (type (or null hash-table) node-children)
                 (type (or null node) node))
        (if node
            (progn
              (push path-component path-spec-components)
              (setf current-node node))
            (let* ((node (or (and node-children
                                  (not (string= "" path-component))
                                  (gethash :dynamic node-children))
                             (return-from find-node)))
                   (binding-name (node-binding node)))
              (declare (type node node)
                       (type keyword binding-name))
              (push (cons binding-name path-component) bindings)
              (push (format nil ":~A" binding-name) path-spec-components)
              (setf current-node node)))))
    (values current-node
            bindings
            path-spec-components)))

(defun compile-router (router-spec &key (fast-dispatch *fast-dispatch*))
  (check-type router-spec list)
  (check-type fast-dispatch boolean)
  (assert (> (length router-spec) 0) nil "Must define at least one route")
  (let ((root (make-node :children (make-hash-table :test #'equal)))
        (defined-routes ()))
    (declare (type node root)
             (type list defined-routes))
    (dolist (route router-spec)
      (destructuring-bind (path-spec route-name) route
        (check-type path-spec string)
        (check-type route-name symbol)
        (assert (and (> (length path-spec) 0)
                     (char= #\/ (char path-spec 0)))
                nil
                "Route pattern ~s must have a leading forward slash: '/'" path-spec)
        (assert (not (member route-name defined-routes))
                nil
                "Handler symbols must be unique. Symbol ~A is defined for more than one path: ~{~A~%~^~}"
                route-name
                (remove-if-not
                 #'(lambda (router-spec)
                     (eq route-name (second router-spec)))
                 router-spec))
        (assert (or (fboundp route-name)
                    (get route-name 'foo.lisp.raven:route-metadata))
                nil
                "Handler symbol ~A is missing either a function value or FOO.LISP.RAVEN:ROUTE-METADATA in its property list (path pattern: ~A)"
                route-name
                path-spec)
        (let ((current-node root)
              (metadata (get route-name 'foo.lisp.raven:route-metadata))
              (defined-path-components ())
              (defined-bindings ()))
          (declare (type node current-node))
          (loop with path-components = (if (string= path-spec "/")
                                           '("")
                                           (cdr (uiop:split-string path-spec :separator "/")))
                for path-component in path-components
                for dynamicp = (and (> (length path-component) 0)
                                    (char= (char path-component 0) #\:))
                for index from 0
                for finalp = (= index (1- (length path-components)))
                for binding = (when dynamicp
                                (let ((kw (intern (subseq path-component 1) :keyword)))
                                  (assert (not (member kw defined-bindings))
                                          nil
                                          "Attempted to define binding more than once for route path: ~A ~A"
                                          kw path-spec)
                                  (check-type kw keyword)
                                  (push kw defined-bindings)
                                  kw))
                for node-key = (if dynamicp :dynamic path-component)
                for node-children = (node-children current-node)
                for next-node = (and node-children
                                     (gethash node-key node-children))
                do (progn
                     (assert (not (string= ":" path-component))
                             nil
                             "Route pattern contains empty dynamic component: ~A" path-spec)
                     (assert (not (search "//" path-spec))
                             nil
                             "Route pattern may not have multiple forward slashes in sequence: ~A" path-spec)
                     (push path-component defined-path-components)
                     (when dynamicp
                       (let* ((current-path (format nil "/~{~A~^/~}" (reverse defined-path-components)))
                              (matching-node (find-node root current-path)))
                         (assert (or (null matching-node)
                                     (eq (car (last defined-bindings))
                                         (node-binding matching-node)))
                                 nil
                                 "Conflicting bindings for node: ~A ~A ~A"
                                 (car (last defined-bindings))
                                 (node-binding matching-node)
                                 path-spec)))
                     (unless next-node
                       (setf next-node (make-node :binding binding))
                       (unless node-children
                         (setf (node-children current-node)
                               (make-hash-table :test #'equal)))
                       (setf (gethash node-key (node-children current-node))
                             next-node))
                     (setf current-node next-node)
                     (when finalp
                       (assert (null (node-route-name current-node))
                               nil
                               "Route already defined for path pattern: ~A" path-spec)
                       (assert (null (member route-name defined-routes))
                               nil
                               "Route symbol associated with multiple path patterns: ~A" route-name)
                       (setf (node-route-name current-node) route-name
                             (node-route-uri-template current-node) (path-spec-to-uri-template path-spec)
                             (node-route-metadata current-node) metadata)
                       (push route-name defined-routes)))))))
    (optimize-trie root)
    (let ((*fast-dispatch* fast-dispatch))
      (declare (type boolean *fast-dispatch*))
      #'(lambda (msg)
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (etypecase msg
            (list
             (let ((cmd (first msg)))
               (ecase cmd
                 (:find-node (destructuring-bind (path)
                                 (rest msg)
                               (check-type path string)
                               (find-node root path)))
                 (:find-route (destructuring-bind (path)
                                  (rest msg)
                                (check-type path string)
                                (find-route root path)))
                 (:dispatch-clack (destructuring-bind (env)
                                      (rest msg)
                                    (declare (type list env))
                                    (dispatch-clack root env))))))
            (keyword
             (ecase msg
               (:get-root
                root)
               (:clack
                #'(lambda (env)
                    (declare (optimize (speed 3) (safety 0) (debug 0))
                             (type node root)
                             (type list env))
                    (dispatch-clack root env))))))))))

(defun dispatch-clack (root env)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type node root)
           (type list env))
  (let ((current-node root)
        (bindings ())
        (path-info (getf env :path-info)))
    (declare (type node current-node)
             (type binding-alist bindings)
             (type (simple-array character (*)) path-info))
    (let ((split-path (if (string= path-info "/")
                          '("")
                          (cdr (uiop:split-string path-info :separator "/")))))
      (declare (type string-list split-path))
      (dolist (path-component split-path)
        (declare (type string path-component))
        (let* ((node-children (node-children current-node))
               (node (and node-children (gethash path-component node-children))))
          (declare (type (or null hash-table) node-children)
                   (type (or null node) node))
          (if node
              (setf current-node node)
              (let* ((node (or (and node-children
                                    (not (equal "" path-component))
                                    (gethash :dynamic node-children))
                               (error 'no-route-error :path-info path-info)))
                     (binding-name (node-binding node)))
                (declare (type node node))
                (push (cons binding-name path-component)
                      bindings)
                (setf current-node node))))))
    (let ((route-name (or (node-route-name current-node)
                          (error 'no-route-error :path-info path-info)))
          (route-metadata (node-route-metadata current-node)))
      (declare (type symbol route-name))
      (setf (getf env :raven.metadata) route-metadata
            (getf env :raven.binding) bindings)
      (funcall (symbol-function route-name) env))))

(defun copy-hash-table (hash-table)
  (declare (type hash-table hash-table))
  (let ((copy (make-hash-table :test #'equal
                               :size (hash-table-count hash-table)
                               :rehash-size 1
                               :rehash-threshold 0)))
    (maphash #'(lambda (key value)
                 (setf (gethash key copy) value))
             hash-table)
    copy))

(defun optimize-trie (node)
  (declare (type node node))
  (when (node-children node)
    (setf (node-children node)
          (copy-hash-table (node-children node)))
    (maphash #'(lambda (key next-node)
                 (declare (ignore key))
                 (optimize-trie next-node))
             (node-children node)))
  (values))

(defun find-route (root path)
  "Returns matching route information for PATH; returns NIL when PATH does not match a
route pattern. Intended for use in development and testing."
  (declare (optimize (speed 0) (safety 3) (debug 3))
           (type node root)
           (type string path))
  (check-type root node)
  (check-type path string)
  (assert (and (> (length path) 0)
               (char= #\/ (char path 0)))
          nil
          "Route path ~s must have a leading forward slash: '/'" path)
  (multiple-value-bind (node bindings path-spec-components)
      (find-node root path)
    (when (and node (node-route-name node))
      (make-route-info
       :path-spec (format nil "/~{~A~^/~}" (nreverse path-spec-components))
       :path path
       :bindings (nreverse bindings)
       :node node))))

(defun path-spec-to-uri-template (pattern)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string pattern))
  (format nil "/~{~A~^/~}"
          (mapcar
           #'(lambda (path-component)
               (declare (type simple-string path-component))
               (if (and (> (length path-component) 0)
                        (char= #\: (char path-component 0)))
                   (format nil "{~A}" (subseq path-component 1))
                   path-component))
           (cdr (uiop:split-string pattern :separator "/")))))

(defun define-route (route-name &rest kwargs &key &allow-other-keys)
  (check-type route-name symbol)
  (let ((meta-sym (getf kwargs :meta))
        (exportp (getf kwargs :export))
        (documentation (getf kwargs :documentation)))
    (check-type meta-sym symbol)
    (check-type exportp boolean)
    (check-type documentation (or null string))
    (assert (null (getf kwargs :route-name))
            nil
            "ROUTE-NAME is not a valid route option")
    (let ((options (loop for (key value) on kwargs by #'cddr
                         unless (member value '(:meta :export :documentation))
                           collect key
                         collect value)))
      (let ((metadata (handler-case (let ((object (apply #'%make-route-metadata meta-sym
                                                         (append options
                                                                 `(:route-name ,route-name)))))
                                      (assert (eq meta-sym (type-of object)))
                                      object)
                        (error (e)
                          (error 'invalid-route-error
                                 :route-name route-name
                                 :options kwargs
                                 :original-error e)))))
        (setf (fdefinition route-name)
              (lambda (env)
                (declare (optimize (speed 3) (safety 0) (debug 0))
                         (type list env))
                (if *fast-dispatch*
                    (%handle-request/fast metadata route-name env)
                    (%handle-request metadata route-name env))))
        (setf (get route-name 'foo.lisp.raven:route-metadata)
              metadata)
        (when documentation
          (setf (documentation route-name 'function)
                documentation))
        (when exportp
          (export route-name))
        route-name))))

(defun define-route-metadata (route-name &rest kwargs &key &allow-other-keys)
  (check-type route-name symbol)
  (let ((meta-sym (getf kwargs :meta))
        (require-fboundp (if (member :fboundp kwargs)
                             (getf kwargs :fboundp)
                             t)))
    (check-type meta-sym symbol)
    (assert (null (getf kwargs :route-name))
            nil
            "ROUTE-NAME is not a valid route option")
    (check-type require-fboundp boolean)
    (when require-fboundp
      (assert (fboundp route-name)
              nil
              "ROUTE-NAME ~A is missing a function value" route-name))
    (assert (not (null kwargs)))
    (let ((options (loop for (key value) on kwargs by #'cddr
                         unless (member value '(:meta :fboundp))
                           collect key
                         collect value)))
      (let ((metadata (handler-case (let ((object (apply #'%make-route-metadata meta-sym
                                                         (append options
                                                                 `(:route-name ,route-name)))))
                                      (assert (eq meta-sym (type-of object)))
                                      object)
                        (error (e)
                          (error 'invalid-route-error
                                 :route-name route-name
                                 :options kwargs
                                 :original-error e)))))
        (setf (get route-name 'foo.lisp.raven:route-metadata)
              metadata)
        route-name))))

(defgeneric %make-route-metadata (metadata &rest kwargs &key &allow-other-keys)
  (:documentation "Validates KWARGS for METADATA, a symbol corresponding to a struct or class,
and returns a new instance of METADATA"))

(defgeneric %handle-request (metadata route-name env)
  (:documentation "Handles a request for the subprotocol associated with METADATA."))

(defgeneric %handle-request/fast (metadata route-name env)
  (:generic-function-class fast-generic-functions:fast-generic-function)
  (:documentation "Handles a request for the subprotocol associated with METADATA; a fast-generic-function."))
