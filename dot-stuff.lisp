;;;; dot-stuff.lisp

(in-package #:dot-stuff)

;;; Graphviz
(defmacro with-new-dot-file ((file &key (name "D") (type :digraph)
					    (if-exists :supersede))
			     &body body)
  "Open FILE for writing, put Graphviz boilerplate there and run BODY with
  *GV-STREAM* bound as a write stream to the file.

  TYPE controls what goes in the boilerplate - the boilerplate is

```
  graph|digraph <NAME> {
    (...)
  }
```

IF-EXISTS is as in OPEN."
  (declare ((member :digraph :graph) type))
  `(with-open-file (*gv-stream* ,file :direction :output
				    :if-exists ,if-exists
				    :external-format :utf-8)
     (format *gv-stream* "~(~a~) ~S {" ,type ,name)
     ,@body
     (format *gv-stream* "~%}")))

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

(defsection @cl-applications (:title "Example applications")
  (draw-some-deps function)
  (draw-package-deps function)
  (draw-system-deps function))

(defun package-deps (expand-list noexpand expand-fn)
  (labels ((expand-step (expand-list deps noexpand)
             (if expand-list
                 (let* ((new-deps (reduce 'append
                                          (mapcar expand-fn expand-list)))
                        (new-pkgs (remove-duplicates (mapcar 'cdr new-deps))))
                   (expand-step (set-difference  new-pkgs (append expand-list noexpand))
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
(gv:draw-package-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) ())
```
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
(gv:draw-system-deps \"/tmp/hunchentoot.gv\" '(hunchentoot) '(cffi cl+ssl))
```
"
  (draw-some-deps file systems-list sinks
                  'asdf/component:component-name
                  'asdf:find-system
                  (lambda (user)
                    (mapcar (lambda (used)
                              (cons user (asdf:find-system used)))
                            (remove-if-not 'stringp (asdf/system:system-depends-on user))))))
