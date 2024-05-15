;;;; package.lisp

(mgl-pax:define-package #:dot-stuff
  (:use #:cl)
  (:import-from #:mgl-pax #:defsection)
  (:nicknames #:gv))


(in-package dot-stuff)
(defsection @index (:title "Dot-stuff")
  (@overview mgl-pax:section))


(defsection @overview
    (:title "Overview")
  "Convenience tools for Graphviz dot file creation."
  (with-new-dot-file mgl-pax:macro)
  (link function)
  (label function)
  (*gv-stream* variable)
  (link-to-many function)
  (@cl-applications mgl-pax:section))
