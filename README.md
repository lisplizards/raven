# raven

> Prefix tree based router for Clack applications

<img title="Raven" alt="Raven" src="raven.svg" width="170px">

## Usage

### Basic example

```lisp
(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root)
     ("/user/:user-id/profile" ,'user-profile))))

(defparameter *web* (funcall *router* :clack))

(defun root (env)
  (declare (ignore env))
  `(200
     (:content-type "text/plain"
      :content-length 13)
     ("Hello, World.")))

(defun user-profile (env)
  (let* ((user-id (cdr (assoc :|user-id| (getf env :raven.binding))))
         (body (format nil "User: ~A" user-id)))
     `(200
       (:content-type "text/plain"
        :content-length ,(length body))
       (,body))))

```

### Route metadata

It is possible to associate some metadata, a struct or class instance, with a route; on dispatch, the router sets the key `RAVEN.METADATA` in the Clack environment to the metadata instance.

Please note that if you rely on this feature and use structs, you should restart the Lisp process when upgrading Raven in case the struct definition has changed.

```lisp
(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root))))

(defparameter *web* (funcall *router* :clack))

(defstruct example
  foo)

(defmethod foo.lisp.raven:%make-route-metadata ((metadata (eql 'example))
                                                &rest kwargs &key &allow-other-keys)
  (destructuring-bind (&key foo &allow-other-keys)
      kwargs
    (check-type foo string)
    (make-example :foo foo)))

(foo.lisp.raven:define-route-metadata 'root
  :meta 'example
  :foo "Quux")

(defun root (env)
  `(200
     (:content-type "text/plain"
      :content-length 13
      :x-foo ,(example-foo (getf env :raven.metadata)))
     ("Hello, World.")))

```

### Sub-protocol handlers

As an alternative to dispatching to standard functions, you can use functions `%HANDLE-REQUEST`, `%HANDLE-REQUEST/FAST`, `%MAKE-ROUTE-METADATA`, and `DEFINE-ROUTE` to define a "sub-protocol"; optionally relies on [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions).

The [Vinland](https://github.com/foo-lisp/vinland) framework is based on use of this feature.

```lisp
(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root))))

(defparameter *web* (funcall *router* :clack))

(defstruct example
  handler)

(defmethod foo.lisp.raven:%make-route-metadata ((metadata (eql 'example))
                                                &rest kwargs &key &allow-other-keys)
  (destructuring-bind (&key handler &allow-other-keys)
      kwargs
    (check-type handler function)
    (make-example :handler handler)))

(defmethod foo.lisp.raven:%handle-request ((metadata example) route-name env)
  (declare (ignorable route-name))
  (let ((request (foo.lisp.lack/request:make-request env))
        (response (lack/response:make-response 200 ())))
    ;; ...
    (funcall (example-handler metadata) request response)
    (lack/response:finalize-response response)))

;; NOTE: EXPORT and DOCUMENTATION are optional.

(foo.lisp.raven:define-route 'root
  :meta 'example
  :export t
  :handler (lambda (request response)
             (declare (ignore request))
             (setf (lack/response:response-headers response)
                   (append (lack/response:response-headers response)
                           '(:content-type "text/plain"
                             :content-length 13)))
             (setf (lack/response:response-body response)
                   "Hello, World."))
  :documentation "Demo route")

```

To avoid repeatedly specifying keyword `META` in your route definitions, you may prefer to define a wrapper function:

```lisp
(defun define-example-route (route-name &rest kwargs &key &allow-other-keys)
  (apply #'foo.lisp.raven:define-route route-name :meta 'example kwargs))

(define-example-route 'root
  :export t
  :handler (lambda (request response)
             (declare (ignore request))
             (setf (lack/response:response-headers response)
                   (append (lack/response:response-headers response)
                           '(:content-type "text/plain"
                             :content-length 13)))
             (setf (lack/response:response-body response)
                   "Hello, World."))
  :documentation "Demo route")

```

Or in case the property-list based syntax does not suit you, you can do something like:

```lisp
(defun define-example-route (route-name &rest rest)
  (ecase (length rest)
    (1 (foo.lisp.raven:define-route route-name
                                    :meta 'example
                                    :export t
                                    :handler (first rest)))
    (2 (foo.lisp.raven:define-route route-name
                                    :meta 'example
                                    :export t
                                    :handler (second rest)
                                    :documentation (first rest)))))

(define-example-route 'root
  "Demo route"
  (lambda (request response)
    (declare (ignore request))
    (setf (lack/response:response-headers response)
          (append (lack/response:response-headers response)
                  '(:content-type "text/plain"
                    :content-length 13)))
    (setf (lack/response:response-body response)
          "Hello, World.")))

```

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.raven)
```

## Contributing

Open an Issue or Pull Request on GitHub.

If you decide to open a Pull Request, please provide context to your changes: describe the impact, what problem it solves for you, and any alternative approaches you considered.

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0