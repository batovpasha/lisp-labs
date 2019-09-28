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

(defun fn(a b c d operation)
  (cond
    ((equal operation "includes") 
      (cond
        ((null b) nil)
        ((equal a (car b)) t)
        (t (fn a (cdr b) nil nil "includes"))
      )
    )

    ((equal operation "diff") 
      (cond
        ((null a) c)
        ((fn (car a) b c nil "includes") (fn (cdr a) b c nil "diff"))
        (t (fn (cdr a) b (cons (car a) c) nil "diff"))
      )
    )

    ((equal operation "union") 
      (cond
        ((null a) b)
        ((fn (car a) b nil nil "includes") (fn (cdr a) b nil nil "union"))
        (t (fn (cdr a) (cons (car a) b) nil nil "union"))
      )
    )

    ((equal operation "intersection") 
      (fn 
        (fn (fn a b nil nil "union") (fn a b nil nil "diff") nil nil "diff") 
        (fn b a nil nil "diff")
        nil
        nil
        "diff"
      )
    )

    ((null operation)
      (fn 
        a 
        (fn 
          b
          (fn
            c
            d
            nil
            nil
            "diff"
          )
          nil 
          nil
          "intersection" 
        ) 
        nil 
        nil 
        "union"
      )   
    )
  )
)


;(print (includes 5 '(1 2 3 4)))
;(print (diff '(1 2) '(1 2 3 4) nil))
;(print (get_union '(3 4 5 6 7) '()))
;(print (get_intersection '(1 2 3 4 5) '(3 4 5 6 7 8)))
(print (fn '(1 2) '(4 5 6) '(5 6 7 8) '() nil))
