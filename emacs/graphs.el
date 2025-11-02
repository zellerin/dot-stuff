;;;###autoload
(defun dynamic-graphs-display-class (class up-or-down)
  "Display graph dependencies of a Common Lisp class.

By default, display all. With universal argument, display
descendants. With two universal arguments, display superclasses
only"
  (interactive "sClass: \nP")
  (message "%s" up-or-down)
  (set-buffer (get-buffer-create "*Classes*"))
  (kill-region (point-min) (point-max))
  (insert (sly-eval `(,(cond
                        ((null up-or-down) 'dot-stuff::draw-classes)
                        ((equal up-or-down '(4)) 'dot-stuff::draw-classes-up)
                        (t 'dot-stuff::draw-classes-down))
                      nil '(,(intern (upcase class))))))
  (dynamic-graphs-display-graph-buffer (list (replace-regexp-in-string ".*:+" "" (upcase class)))
                                       '(remove-cycles boxize "N[class==\"sink\"]{style=\"filled\",fillcolor=\"lightblue\"}"
                                                       "N[class==\"source\"]{style=\"filled\",fillcolor=\"lightgreen\"}"
                                                       "E[tail.class==\"sink\"]{style=\"dotted\",constraint=\"false\"}"
                                                       "BEG_G{rankdir=\"LR\"}"))
  (dynamic-graphs-set-engine "dot"))
