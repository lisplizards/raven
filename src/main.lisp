;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.raven)

(declaim (type boolean *fast-dispatch*))
(defvar *fast-dispatch* nil
  "Dynamically bound variable representing whether to use fast-generic-functions on dispatch.")

(declaim (type hash-table *path-generators*))
(defvar *path-generators* (make-hash-table :test #'eq)
  "Dynamically bound variable: a hash-table, with each entry having a route-name keyword as
the key and a path-generator lambda as the value.")

(define-condition router-compilation-error (error)
  ()
  (:documentation "Base condition class for router compilation failures."))

(define-condition route-metadata-error (error)
  ((route-name :initarg :route-name
               :type symbol
               :documentation "Route-name symbol for which an error was signalled.")
   (options :initarg :options
            :type list
            :documentation "List of subprotocol metadata options.")
   (original-error :initarg :original-error
                   :documentation "The original error signalled when attempting to construct the
route subprotocol metadata."))
  (:report (lambda (condition stream)
             (with-slots (route-name options original-error) condition
               (format stream "Invalid route: ~A~%Options: ~A~%Original error: ~A"
                       route-name
                       options
                       original-error))))
  (:documentation "Error condition signalled when when the route metadata instance cannot be constructed."))

(define-condition no-path-generator-error (error)
  ((route-name :initarg :route-name
               :type keyword
               :documentation "Unrecognized route-name keyword"))
  (:report (lambda (condition stream)
             (with-slots (route-name)
                 condition
               (format stream "No route path-generator defined for keyword: ~A" route-name))))
  (:documentation "Error signalled when attempting to lookup the path generator for an unknown route-name symbol ."))

(define-condition no-route-function-error (router-compilation-error)
  ((route-name :initarg :route-name
               :type symbol
               :documentation "The route-name symbol that lacks a function value"))
  (:report (lambda (condition stream)
             (with-slots (route-name)
                 condition
               (format stream "Route-name symbol is missing a function value: ~A" route-name))))
  (:documentation "Error signalled when attempting to compile a route with a route name symbol without a function value."))

(define-condition no-route-error (simple-error)
  ((path-info :initarg :path-info
              :type (simple-array character (*))
              :documentation "The dispatched path for which no handler could be matched."))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No route handler for request")))
  (:documentation "Error signalled when the route cannot be matched to a handler on dispatch."))

(defstruct (route-info
            (:print-function
             (lambda (struct stream depth)
               (declare (ignore depth))
               (format stream "Route~%-----~%Name: ~A~%Pattern: ~A~%Metadata: ~A~%Documentation: ~A~%"
                       (route-info-route-name struct)
                       (route-info-path-spec struct)
                       (route-info-metadata struct)
                       (documentation (route-info-route-name struct) 'function)))))
  "Route information for a specific path handler."
  route-name
  path-spec
  metadata)

(defstruct node "Routing trie node: contains any children nodes and any routing information."
   (binding nil :type (or null keyword))
   (route-info nil :type (or null route-info))
   (children nil :type (or null hash-table)))

(defstruct (find-route-result
            (:print-function
             (lambda (struct stream depth)
               (declare (ignore depth))
               (let ((route-info (find-route-result-route-info struct)))
                 (format stream "Route~%-----~%Name: ~A~%Pattern: ~A~%Bindings: ~A~%~\
Path: ~A~%Metadata: ~A~%Documentation: ~A~%"
                         (route-info-route-name route-info)
                         (route-info-path-spec route-info)
                         (find-route-result-bindings struct)
                         (find-route-result-path struct)
                         (route-info-metadata route-info)
                         (documentation (route-info-route-name route-info) 'function))))))
  "Struct returned from FIND-ROUTE, containing ROUTE-INFO, the queried PATH, and any path BINDINGS."
  route-info
  path
  bindings)

(defmacro compile-path-generator (path-components)
  "Macro that expands to a lambda function, which, when called with the appropriate
path binding keyword arguments, returns a path string.

PATH-COMPONENTS is a list of strings and keywords: strings represent static path
components, whereas keywords represent dynamic components for which a keyword
parameter will be defined in the lambda-list of the returned function.

Example:
If PATH-COMPONENTS evaluates to a list like `(\"widgets\" :|widget-id|)', then a
lambda function is returned that has keyword parameter :|widget-id|, and when called
like `(funcall generator :|widget-id| \"xyz\")', returns string \"/widgets/xyz\"."
  `(let* ((components ,path-components)
          (format-string (format nil "/~{~A~^/~}"
                                 (mapcar (lambda (item)
                                           (if (keywordp item)
                                               "~A"
                                               item))
                                         components)))
          (keywords (remove-if-not #'keywordp components))
          (bindings (mapcar
                     (lambda (component)
                       (intern (string component)))
                     keywords)))
     (compile
      nil
      `(lambda (&key ,@bindings)
         ,@(mapcar (lambda (component)
                     `(assert ,(intern (string component))
                              nil
                              "Keyword parameter ~A cannot be NIL"
                              ,component))
                   keywords)
         (format nil ,format-string ,@bindings)))))

(defmacro route-path (route-name &rest kwargs &key &allow-other-keys)
  "Calls the path-generator for ROUTE-NAME with KWARGS, returning a path string.
  Signals NO-PATH-GENERATOR-ERROR when ROUTE-NAME is not recognized; a macro."
  (let ((gensym-route-name (gensym "route-name")))
    `(let* ((,gensym-route-name (intern (string ,route-name) :keyword)))
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type keyword ,gensym-route-name))
       (multiple-value-bind (generator foundp)
           (gethash ,gensym-route-name foo.lisp.raven::*path-generators*)
         (declare (type (or null function) generator)
                  (type boolean foundp))
         (unless foundp
           (error 'no-path-generator-error
                  :route-name ,gensym-route-name))
         (the string (funcall (the function generator) ,@kwargs))))))

(declaim (ftype (function (list &key (:fast-dispatch boolean)) function) compile-router))
(defun compile-router (router-spec &key (fast-dispatch *fast-dispatch*))
  "Returns a lambda function that can return a Clack application or introspect
routes. ROUTER-SPEC is a list of two-item lists, the first being the path-pattern
string and the second being the route-name symbol."
  (declare (type list router-spec)
           (type boolean fast-dispatch))
  (check-type router-spec list)
  (check-type fast-dispatch boolean)
  (assert (> (length router-spec) 0) nil "Must define at least one route")
  (let ((root (make-node :children (make-hash-table :test #'equal)))
        (path-generators (make-hash-table :test #'eq))
        (defined-routes ()))
    (declare (type node root)
             (type list defined-routes))
    (dolist (route router-spec)
      (destructuring-bind (path-spec route-name)
          route
        (check-type path-spec string)
        (check-type route-name symbol)
        (assert (and (> (length path-spec) 0)
                     (char= #\/ (char path-spec 0)))
                nil
                "Route pattern ~s must have a leading forward slash: '/'" path-spec)
        (let ((route-name-kw (intern (string route-name) :keyword)))
          (assert (not (member route-name-kw defined-routes :test #'eq))
                  nil
                  "Handler symbols must be unique. ~A is defined for more than one path: ~{~A~%~^~}"
                  route-name
                  (remove-if-not
                   #'(lambda (router-spec)
                       (eq route-name (second router-spec)))
                   router-spec))
          (unless (fboundp route-name)
            (error 'no-route-function-error
                   :route-name route-name))
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
                       (push (if dynamicp binding path-component)
                             defined-path-components)
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
                         (assert (null (node-route-info current-node))
                                 nil
                                 "Route already defined for path pattern: ~A" path-spec)
                         (assert (null (member route-name-kw defined-routes :test #'eq))
                                 nil
                                 "Route symbol associated with multiple path patterns: ~A" route-name)
                         (setf (node-route-info current-node)
                               (make-route-info :route-name route-name
                                                :path-spec path-spec
                                                :metadata metadata)
                               (gethash route-name-kw path-generators)
                               (compile-path-generator (reverse defined-path-components)))
                         (push route-name-kw defined-routes))))))))
    (optimize-trie root)
    (lambda (msg)
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
             (:list-routes (destructuring-bind (prefix)
                               (rest msg)
                             (check-type prefix string)
                             (list-routes root prefix)))
             (:print-routes (destructuring-bind (prefix)
                                (rest msg)
                              (check-type prefix string)
                              (print-routes root prefix))))))
        (keyword
         (ecase msg
           (:get-root
            root)
           (:clack
            (lambda (env)
              (declare (optimize (speed 3) (safety 0) (debug 0))
                       (type node root)
                       (type list env))
              (let ((*fast-dispatch* fast-dispatch)
                    (*path-generators* path-generators))
                (declare (type boolean *fast-dispatch*)
                         (type hash-table *path-generators*))
                (dispatch root env))))))))))

(declaim (ftype (function (node list) list) dispatch))
(defun dispatch (root env)
  "Dispatches the Clack request, traversing the prefix tree and returning a Clack
response list. Signals NO-ROUTE-ERROR when no handler exists for the route."
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
    (let ((route-info (or (node-route-info current-node)
                          (error 'no-route-error :path-info path-info))))
      (declare (type route-info route-info))
      (setf (getf env :raven.binding) bindings)
      (funcall (symbol-function (the symbol (route-info-route-name route-info)))
               env))))

(declaim (ftype (function (node string) (values (or null node) list string-list)) find-node))
(defun find-node (root path)
  "Looks up a NODE for PATH. When found, returns three values:
1. the NODE for the given PATH
2. a list of any binding keywords for the matched path
3. a list of strings representing

Returns NIL if no node exists for PATH."
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
                             (return-from find-node
                               (values nil nil nil)))))
              (declare (type node node))
              (let ((binding-name (node-binding node)))
                (declare (type keyword binding-name))
                (push (cons binding-name path-component)
                      bindings)
                (push (format nil ":~A" binding-name) path-spec-components))
              (setf current-node node)))))
    (values current-node
            bindings
            path-spec-components)))

(declaim (ftype (function (hash-table) hash-table) copy-hash-table))
(defun copy-hash-table (hash-table)
  "Returns an optimized copy of HASH-TABLE."
  (declare (type hash-table hash-table))
  (let ((copy (make-hash-table :test #'equal
                               :size (hash-table-count hash-table)
                               :rehash-size 1
                               :rehash-threshold 0)))
    (maphash #'(lambda (key value)
                 (setf (gethash key copy) value))
             hash-table)
    copy))

(declaim (ftype (function (node) (values)) optimize-trie))
(defun optimize-trie (node)
  "Recursively descends NODE, a prefix tree, replacing children hash tables
with optimized copies."
  (declare (type node node))
  (when (node-children node)
    (setf (node-children node)
          (copy-hash-table (node-children node)))
    (maphash #'(lambda (key next-node)
                 (declare (ignore key))
                 (optimize-trie next-node))
             (node-children node)))
  (values))

(declaim (ftype (function (node string) list) print-routes))
(defun print-routes (root prefix)
  "Print all ROUTE-INFO structs matching path-prefix PREFIX."
  (format t "~{~^~A~%~}" (list-routes root prefix)))

(declaim (ftype (function (node string) list) list-routes))
(defun list-routes (root prefix)
  "Returns a list of ROUTE-INFO structs matching path-prefix PREFIX."
  (declare (type node root)
           (type string prefix))
  (check-type root node)
  (check-type prefix string)
  (let ((routes ()))
    (multiple-value-bind (node bindings path-spec-components)
        (find-node root (if (string= "/" prefix)
                            ""
                            (progn
                              (assert (and (> (length prefix) 0)
                                           (char= #\/ (char prefix 0)))
                                      nil
                                      "Route path prefix ~s must have a leading forward slash: '/'" prefix)
                              prefix)))
      (declare (ignore bindings path-spec-components))
      (unless node
        (return-from list-routes))
      (labels ((traverse (node)
                 (when (node-route-info node)
                   (push (node-route-info node) routes))
                 (when (node-children node)
                   (maphash
                    (lambda (key value)
                      (declare (ignore key))
                      (traverse value))
                    (node-children node)))))
        (traverse node)
        (nreverse routes)))))

(declaim (ftype (function (node string) (or null find-route-result)) find-route))
(defun find-route (root path)
  "Returns matching route information for PATH; returns NIL when PATH does not
match a route pattern; a development utility."
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
    (declare (ignore path-spec-components))
    (let ((route-info (or (and node (node-route-info node))
                          (return-from find-route))))
      (declare (type route-info route-info))
      (make-find-route-result
       :route-info route-info
       :path path
       :bindings (nreverse bindings)))))

(declaim (ftype (function (symbol &rest t &key &allow-other-keys) symbol) define-route))
(defun define-route (route-name &rest kwargs &key &allow-other-keys)
  "Parses a metadata object from KWARGS and sets the metadata object in the symbol
plist of ROUTE-NAME and assigns a function value to ROUTE-NAME, the route handler;
a development utility."
  (declare (type symbol route-name)
           (type t kwargs))
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
                                      (assert (eq meta-sym (type-of object))
                                              nil
                                              "The result of %MAKE-ROUTE-METADATA for route ~A must be of type ~A"
                                              route-name
                                              meta-sym)
                                      object)
                        (error (e)
                          (error 'route-metadata-error
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

(declaim (ftype (function (symbol &rest t &key &allow-other-keys) symbol) define-route-metadata))
(defun define-route-metadata (route-name &rest kwargs &key &allow-other-keys)
  "Parses a metadata object from KWARGS and sets the metadata object in the
plist of ROUTE-NAME."
  (declare (type symbol route-name)
           (type t kwargs))
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
                                      (assert (eq meta-sym (type-of object))
                                              nil
                                              "The result of %MAKE-ROUTE-METADATA for route ~A must be of type ~A"
                                              route-name
                                              meta-sym)
                                      object)
                        (error (e)
                          (error 'route-metadata-error
                                 :route-name route-name
                                 :options kwargs
                                 :original-error e)))))
        (setf (get route-name 'foo.lisp.raven:route-metadata)
              metadata)
        route-name))))

(defgeneric %make-route-metadata (metadata &rest kwargs &key &allow-other-keys)
  (:documentation "Validates KWARGS for METADATA, a symbol corresponding to a struct
or class, and returns a new instance of METADATA"))

(defgeneric %handle-request (metadata route-name env)
  (:documentation "Handles a request for the subprotocol associated with METADATA."))

(defgeneric %handle-request/fast (metadata route-name env)
  (:generic-function-class fast-generic-functions:fast-generic-function)
  (:documentation "Handles a request for the subprotocol associated with METADATA;
an alternative to %HANDLE-REQUEST and a fast-generic-function."))
