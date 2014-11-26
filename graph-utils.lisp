; Converts anything to a string that graphviz can handle
(defun dot-name (x)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string x)))

; Max label length in a node
(defparameter *max-label-length* 30)

; make a graphviz label for an expression
(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string x :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

; convert nodes to graphviz syntax
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

; convert edges to graphbiz syntax
(defun edges->dot (nodes)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

; convert a graph to graphviz syntax
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

; convert graphviz data to a PNG image
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

; make a PNG from a graph
(defun graph->png (fname nodes edges)
  (dot->png fname (lambda ()
                    (graph->dot nodes edges))))

; generate graphviz syntax for undirected edges
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

; generate graphviz syntax for an undirected graph
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

; generate a PNG image for an undirected graph
(defun ugraph->png (fname nodes edges)
  (dot->png fname (lambda ()
                    (ugraph->dot nodes edges))))
