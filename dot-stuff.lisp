;;;; dot-stuff.lisp

(in-package #:dot-stuff)

;;; Graphviz

(defun write-somewhere (output build-fn &key if-exists)
  "Call WRITE-FN to write to an output. Output can be a stream, nil (denoting string output) or file name (string or pathname).

This should be maybe somewhere in Alexandria or something?

```cl-transcribe
  (write-somewhere nil 'terpri)
```

```cl-transcribe
  (write-somewhere *standard-output* 'terpri)
```

```cl-transcribe
  (write-somewhere file 'terpri)
```

"
  (etypecase output
    ((or string pathname)
     (with-open-file (out output :direction :output
                                 :if-exists if-exists
                                 :external-format :utf-8)
       (funcall build-fn out)))
    (null
     (with-output-to-string (out)
       (funcall build-fn out)))
    ((satisfies streamp)        ; is there no base class for streams?
     (funcall build-fn output))
))


(defmacro with-new-dot-file ((output &key (name "D") (type :digraph)
				     (if-exists :supersede))
			     &body body)
  "Print Graphviz boilerplate and output of BODY with
  *GV-STREAM* bound as a write stream to the OUTPUT (stream, file or return value).

  TYPE controls what goes in the boilerplate - the boilerplate is

```
  graph|digraph <NAME> {
    (...)
  }
```

IF-EXISTS is as in OPEN."
  (declare ((member :digraph :graph) type))
  `(write-somewhere ,output
                    (lambda (*gv-stream*)
                      (format *gv-stream* "~(~a~) ~S {" ,type ,name)
                      ,@body
                      (format *gv-stream* "~%}"))
                    :if-exists ,if-exists))

(defvar *gv-stream* *standard-output*
  "Write stream bound in WITH-NEW-DOT-FILE. It allows user to write additional content to the graphviz file.")

(defun link (from to &optional (type 'link))
  "Print a graphviz link to *GV-STREAM*. FROM and TO are node names. TYPE is used
as \"type\" attribute to the link"
  (format *gv-stream* "~&~8T\"~a\" -> \"~a\" [type=\"~A\"]" from to type))

(defun link-to-many (from to &optional (type 'link))
  "Print a graphviz link from one node (FROM) to several (TO, a list) to the
*GV-STREAM*."
  (format *gv-stream* "~&~8T~s -> {~@<~{~s~^,~_~}}  ~_[type=\"~a\"]~:>" from to type))

(defun label (id name attrs)
  "Print a graphviz node ID with LABEL and other attributes to *GV-STREAM*.

ATTRs should be a list of name-values (plist style)."
  (format *gv-stream* "~&~4T\"~a\"~8T[label=\"~a\"~{, ~(~A~)=~S~}]~%"
	  id name attrs))

(defsection @cl-applications (:title "Example applications inside CL")
  (draw-some-deps function)
  (draw-package-deps function)
  (draw-system-deps function))

(defun package-deps (expand-list noexpand expand-fn)
  (labels ((expand-step (expand-list deps noexpand)
             (if expand-list
                 (let* ((new-deps (reduce 'append
                                          (mapcar expand-fn expand-list)))
                        (new-pkgs (remove-duplicates (concatenate 'list
                                                                  (mapcar 'cdr new-deps)
                                                                  (mapcar 'car new-deps)))))
                   (expand-step (set-difference new-pkgs (append expand-list noexpand))
                                (append deps new-deps)
                                (append noexpand expand-list)))
                 deps)))
    (expand-step expand-list nil noexpand)))

(defvar *default-known* '(cl cffi mgl-pax))

(defun draw-some-deps (file root-node-names sink-names to-name-fn from-name-fn expand-fn)
  "Make a graphviz FILE containing dependencies generated from ROOT-NODE-NAMES using EXPAND-FN.

EXPAND-FN takes an existing node and returns list of pairs to add.

SINK-NAMES are no longer expanded.

TO-NAME-FN and FROM-NAME-FN are used to convert node objects to name or create
node object from its name.

The root and sink nodes have set graphiv attribute class to \"source\" or \"sink\".

See examples DRAW-SYSTEM-DEPS and DRAW-PACKAGE-DEPS."
  (with-new-dot-file (file :name "packages")
    (dolist (sink sink-names)
      (setf sink (funcall to-name-fn (funcall from-name-fn sink)))
      (label sink sink '("class" "sink")))
    (dolist (source root-node-names)
      (setf source (funcall to-name-fn (funcall from-name-fn source)))
      (label source source '("class" "source")))
    (dolist (pair (package-deps (mapcar from-name-fn root-node-names)
                                (mapcar from-name-fn sink-names)
                                expand-fn))
      (link (funcall to-name-fn (car pair))
            (funcall to-name-fn (cdr pair))))))

(defun draw-package-deps (file package-list known)
  "Draw dependencies of CL packages from PACKAGE-LIST into graphviz FILE.

The COMMON-LISP package is not shown as a dependency for simple view.

Example:

```
(gv:draw-package-deps \"/tmp/hunchentoot.gv\" '(drakma hunchentoot) ())
```

![](img/h-packages.png)
"
  (draw-some-deps file package-list known #'package-name #'find-package
                  (lambda (user)
                    (mapcar (lambda (used) (cons user used))
                            (remove (find-package 'cl)
                                    (package-use-list user))))))

(defun draw-system-deps (file systems-list sinks)
  "Draw dependencies of an ASDF systems in SYSTEMS-LIST to a graphviz FILE.

Example: draw dependencies of Hunchentoot asdf package, do not expand cffi and cl+ssl.

```
(gv:draw-system-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) '(cl+ssl))
```

![](img/hunchentoot.png)
"
  (draw-some-deps file systems-list sinks
                  'asdf/component:component-name
                  'asdf:find-system
                  (lambda (user)
                    (mapcar (lambda (used)
                              (cons user (asdf:find-system used)))
                            (remove-if-not 'stringp (asdf/system:system-depends-on user))))))


(defun draw-classes-up (file classes sinks)
  "Draw dependencies of an class to graphviz FILE.

```
(gv:draw-system-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) '(cl+ssl))
```

![](img/hunchentoot.png)
"
  (draw-some-deps file classes sinks
                  'class-name
                  'find-class
                  (lambda (user)
                    (mapcar (lambda (used)
                              (cons user used))
                            (closer-mop:class-direct-superclasses user)))))

(defun draw-classes-down (file classes sinks)
  "Draw dependencies of an class to graphviz FILE.

```
(gv:draw-system-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) '(cl+ssl))
```

![](img/hunchentoot.png)
"
  (draw-some-deps file classes sinks
                  'class-name
                  'find-class
                  (lambda (user)
                    (mapcar (lambda (used)
                              (cons user used))
                            (closer-mop:class-direct-subclasses user)))))

(defun draw-fns-up (file functions &optional sinks)
  "Draw users of a FUNCTION to graphviz FILE.

```
```

![](img/hunchentoot.png)
"
  (draw-some-deps file FUNCTIONS sinks
                  'identity
                  'identity
                  (lambda (used)
                    (mapcar (lambda (user)
                              (cons used (car user)))
                            (sb-introspect:who-calls used)))))

(defun draw-classes (file classes &optional (sinks '(standard-object)))
  "Draw dependencies of an class to graphviz FILE.

```
(gv:draw-system-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) '(cl+ssl))
```

![](img/hunchentoot.png)
"
  (draw-some-deps file classes sinks
                  'class-name
                  'find-class
                  (lambda (user)
                    (mapcar (lambda (used)
                              (cons user used))
                            (append (closer-mop:class-direct-subclasses user)
                                    (closer-mop:class-direct-superclasses user))))))
