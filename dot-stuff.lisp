;;;; dot-stuff.lisp

(in-package #:dot-stuff)

(define-section @dot
  "Convenience tools for dot file creation."
  (with-new-dot-file macro)
  (link) (label) (*gv-stream* variable)
  (link-to-many))

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
  (format *gv-stream* "~&~8T\"~s -> {~@<~{~s~^,~_~}}  ~_[type=\"~a\"]~:>" from to type))

(defun label (id name attrs)
  "Print a graphviz label.

ATTRs should be a list of name-value lists."
  (format *gv-stream* "~&~4T\"~a\"~8T[label=\"~a\"~{, ~(~A~)=~S~}]~%"
	  id name attrs))
