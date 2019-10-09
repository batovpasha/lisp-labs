(defun create_new_list(lst1 lst2 lst3)
  (list (car lst1) (third lst2) (sixth lst3))
)

(write
  (create_new_list '((PI) V (H J K)) '(R YU (H KJ KL)) '(U II OO LL PP (3 4 5)))
)