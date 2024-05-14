;;;; dot-stuff.lisp

(in-package #:dot-stuff)

(mgl-pax:defsection @dot ()
  "Convenience tools for Graphviz dot file creation."
  (with-new-dot-file macro)
  (link function) (label function) (*gv-stream* variable)
  (link-to-many function))

;;; Graphviz
(defmacro with-new-dot-file ((file &key (name "D") (type :digraph)
					    (if-exists :supersede))
			     &body body)
  "Open FILE for writing, put boilerplate there and run BODY with
  STREAM bound as a write stream to the file.

  The boilerplate is
  graph|digraph <NAME> {
    (...)
  }"
  (declare ((member :digraph :graph) type))
  `(with-open-file (*gv-stream* ,file :direction :output
				    :if-exists ,if-exists
				    :external-format :utf-8)
     (format *gv-stream* "~(~a~) ~S {" ,type ,name)
     ,@body
     (format *gv-stream* "~%}")))

(defvar *gv-stream* *standard-output*
  "String to user for graphviz print operations.")

(defun link (from to &optional (type 'link))
  "Print a graphviz link"
  (format *gv-stream* "~&~8T\"~a\" -> \"~a\" [type=\"~A\"]" from to type))

(defun link-to-many (from to &optional (type 'link))
  (format *gv-stream* "~&~8T~s -> {~@<~{~s~^,~_~}}  ~_[type=\"~a\"]~:>" from to type))

(defun label (id name attrs)
  "Print a graphviz label.

ATTRs should be a list of name-value lists."
  (format *gv-stream* "~&~4T\"~a\"~8T[label=\"~a\"~{, ~(~A~)=~S~}]~%"
	  id name attrs))

(defvar *package-traverse-fn* #'package-use-list)

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
  (draw-some-deps file package-list known #'package-name #'find-package
                                                  (lambda (user)
                                  (mapcar (lambda (used) (cons user used))
                                          (package-use-list user)))))

(defun draw-system-deps (file package-list known)
  (draw-some-deps file package-list known
                  'asdf/component:component-name
                  'asdf:find-system
                  (lambda (user)
                    (mapcar (lambda (used)
                                            (cons user (asdf:find-system used)))
                            (remove-if-not 'stringp (asdf/system:system-depends-on user))))))
