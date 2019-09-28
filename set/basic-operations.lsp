(defun includes(element set)
  (cond
    ((null set) nil)
    ((equal element (car set)) t)
    (t (includes element (cdr set)))
  )
)

(defun get_diff(a b result)
  (cond
    ((null a) result)
    ((includes (car a) b) (get_diff (cdr a) b result))
    (t (get_diff (cdr a) b (cons (car a) result)))
  )
)

(defun get_union(a b)
  (cond
    ((null a) b)
    ((includes (car a) b) (get_union (cdr a) b))
    (t (get_union (cdr a) (cons (car a) b)))
  )
)

(defun get_intersection(a b)
  (get_diff
    (get_diff (get_union a b) (get_diff a b nil) nil)
    (get_diff b a nil)
    nil
  )
)