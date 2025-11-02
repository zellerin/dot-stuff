;;;###autoload
(defun dynamic-graphs-display-class (class up-or-down)
  "Display graph dependencies of a Common Lisp class.

By default, display all. With universal argument, display
descendants. With two universal arguments, display superclasses
only"
  (interactive "sClass: \nP")
  (set-buffer (get-buffer-create "*Classes*"))
  (kill-region (point-min) (point-max))
  (insert (sly-eval `(,(cond
                        ((null up-or-down) 'dot-stuff:draw-classes)
                        ((equal up-or-down '(4)) 'dot-stuff:draw-classes-up)
                        (t 'dot-stuff:draw-classes-down))
                      nil '(,(intern (upcase class))))))
  (dynamic-graphs-display-graph-buffer (list (replace-regexp-in-string ".*:+" "" (upcase class)))
                                       '(remove-cycles boxize "N[class==\"sink\"]{style=\"filled\",fillcolor=\"lightblue\"}"
                                                       "N[class==\"source\"]{style=\"filled\",fillcolor=\"lightgreen\"}"
                                                       "N[]{URL=name, id=name}"
                                                       "E[tail.name==\"STANDARD-OBJECT\"]{style=\"invis\",constraint=\"false\"}"
                                                       "N[name==\"STANDARD-OBJECT\"]{style=\"invis\"}"
                                                       "BEG_G{rankdir=\"LR\"}"
                                                       node-refs))
  (dynamic-graphs-set-engine "dot"))

;;;###autoload
(defun dynamic-graphs-display-packages (package)
  "Display a graph of dependencies of a Common Lisp package"
  (interactive "sPackage: ")
  (set-buffer (get-buffer-create "*Classes*"))
  (kill-region (point-min) (point-max))
  (insert (sly-eval `(dot-stuff:draw-package-deps
                      nil '(,(upcase package)))))
  (dynamic-graphs-display-graph-buffer (list (upcase package))
                                       '(remove-cycles boxize "N[class==\"sink\"]{style=\"filled\",fillcolor=\"lightblue\"}"
                                                       "N[class==\"source\"]{style=\"filled\",fillcolor=\"lightgreen\"}"
                                                       "N[]{URL=name, id=name}"
                                                       "N[name==\"STANDARD-OBJECT\"]{style=\"invis\"}"
                                                       "BEG_G{rankdir=\"LR\"}"
                                                       node-refs))
  (dynamic-graphs-set-engine "dot"))

;;;###autoload
(defun dynamic-graphs-display-function (function)
  "Display graph dependencies of a Common Lisp class.

By default, display all. With universal argument, display
descendants. With two universal arguments, display superclasses
only"
  (interactive "sFunction: ")
  (set-buffer (get-buffer-create "*Functions*"))
  (kill-region (point-min) (point-max))
  (insert (sly-eval `(dot-stuff:draw-fns-up nil '(,(intern (upcase function))))))
  (dynamic-graphs-display-graph-buffer (list (replace-regexp-in-string ".*:+" "" (upcase function)))
                                       '(remove-cycles boxize "N[class==\"sink\"]{style=\"filled\",fillcolor=\"lightblue\"}"
                                                       "N[class==\"source\"]{style=\"filled\",fillcolor=\"lightgreen\"}"
                                                       "N[]{URL=name, id=name}"
                                                       "E[tail.name==\"STANDARD-OBJECT\"]{style=\"invis\",constraint=\"false\"}"
                                                       "N[name==\"STANDARD-OBJECT\"]{style=\"invis\"}"
                                                       "BEG_G{rankdir=\"LR\"}"
                                                       node-refs))
  (dynamic-graphs-set-engine "dot"))

;;;###autoload
(defun dynamic-graphs-display-system (system)
  "Display graph dependencies of a Common Lisp system.

By default, display all. With universal argument, display
descendants. With two universal arguments, display superclasses
only"
  (interactive "sSystem: ")
  (set-buffer (get-buffer-create "*Functions*"))
  (kill-region (point-min) (point-max))
  (insert (sly-eval `(dot-stuff:draw-system-deps nil '(,system))))
  (dynamic-graphs-display-graph-buffer (list (replace-regexp-in-string ".*:+" "" system))
                                       '(remove-cycles boxize "N[class==\"sink\"]{style=\"filled\",fillcolor=\"lightblue\"}"
                                                       "N[class==\"source\"]{style=\"filled\",fillcolor=\"lightgreen\"}"
                                                       "N[]{URL=name, id=name}"
                                                       "E[tail.name==\"STANDARD-OBJECT\"]{style=\"invis\",constraint=\"false\"}"
                                                       "N[name==\"STANDARD-OBJECT\"]{style=\"invis\"}"
                                                       "BEG_G{rankdir=\"LR\"}"
                                                       node-refs))
  (dynamic-graphs-set-engine "dot"))
