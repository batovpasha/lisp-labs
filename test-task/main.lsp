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

    ; Function: A + (B * (C - D))
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

; Tests

(if
  (equal (fn '(1 2 3) '() '(3 4) '(1 5) nil) '(3 2 1))
  (print "Test 1 passed!")
  (print "Test 1 failed!")
)

(if
  (equal (fn '(1 2 3) '(2) '(3 4) '(1 5) nil) '(3 2 1))
  (print "Test 2 passed!")
  (print "Test 2 failed!")
)

(if
  (equal (fn '(1 2 3) '(4) '(3 4) '(1 5) nil) '(3 2 1 4))
  (print "Test 3 passed!")
  (print "Test 3 failed!")
)

(if
  (equal (fn '(1 2 3) '(4 5) '(5 4) '(1 5) nil) '(3 2 1 4))
  (print "Test 4 passed!")
  (print "Test 4 failed!")
)

(if
  (equal (fn '(1 2 3) '(4 5 6) '(5 6 4) '(1 5) nil) '(3 2 1 4 6))
  (print "Test 5 passed!")
  (print "Test 5 failed!")
)

(if
  (equal (fn '() '(4 5 6) '(5) '(1 5) nil) nil)
  (print "Test 6 passed!")
  (print "Test 6 failed!")
)

(if
  (equal (fn '() '(4 5 6) '(5) '(1) nil) '(5))
  (print "Test 7 passed!")
  (print "Test 7 failed!")
)

(if
  (equal (fn '(1 2) '(4 5 6) '(5 6 7 8) '(1) nil) '(2 1 6 5))
  (print "Test 8 passed!")
  (print "Test 8 failed!")
)