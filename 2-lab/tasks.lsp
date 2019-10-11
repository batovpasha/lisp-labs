; Описать функцию, которая находила бы сумму всех числовых элементов списка с учетом наличия подсписков. 
; Пример : для списка „(1 ((2 3) 4) 5 6) результатом будет 21.

(defun flat_sum(lst &optional (sum 0))
  (cond
    ((null lst) sum)
    ((not (atom (car lst))) (flat_sum (cdr lst) (flat_sum (car lst) sum)))
    (t (flat_sum (cdr lst) (+ (car lst) sum)))
  )
)

(print (flat_sum '(1 ((2 3) 4) 5 6)))
(print (flat_sum '((1 (2)) (3 4) 5 (((((6))))))))

; Сортировка методом прямого выбора.

(defun find_min_element(lst &optional (curr_min_element (car lst)))
  (cond
    ((null lst) curr_min_element)
    ((< (car lst) curr_min_element) (find_min_element (cdr lst) (car lst)))
    (t (find_min_element (cdr lst) curr_min_element))
  )
)

(defun find_max_element(lst &optional (curr_max_element (car lst)))
  (cond
    ((null lst) curr_max_element)
    ((> (car lst) curr_max_element) (find_max_element (cdr lst) (car lst)))
    (t (find_max_element (cdr lst) curr_max_element))
  )
)

(defun remove_element(el lst)
  (cond
    ((null lst) nil)
    ((equal el (car lst)) (cdr lst))
    (t (cons (car lst) (remove_element el (cdr lst))))
  )
)

(defun selection_sort_asc(lst)
  (cond
    ((null lst) nil)
    (t (cons (find_min_element lst) (selection_sort_asc (remove_element (find_min_element lst) lst))))
  )
)

(defun selection_sort_desc(lst)
  (cond
    ((null lst) nil)
    (t (cons (find_max_element lst) (selection_sort_desc (remove_element (find_max_element lst) lst))))
  )
)

(print (selection_sort_desc '(10 49 -120 -43 95648 -42344)))
(print (selection_sort_asc '(10 49 -120 -43 95648 -42344)))

; Написать программу объединения двух отсортированных списков в один. 
; При этом порядок сортировки в списке-результате должен сохраняться.

(defun join_lists(lst1 lst2)
  (cond
    ((null lst1) lst2)
    (t (join_lists (cdr lst1) (cons (car lst1) lst2)))
  )
)

(defun join_sorted_lists_with_order_saving(lst1 lst2)
  (cond
    (
      (and 
        (equal lst1 (selection_sort_asc lst1)) 
        (equal lst2 (selection_sort_asc lst2))
      ) 
      (selection_sort_asc (join_lists lst1 lst2))
    )
    
    (
      (and 
        (equal lst1 (selection_sort_desc lst1)) 
        (equal lst2 (selection_sort_desc lst2))
      ) 
      (selection_sort_desc (join_lists lst1 lst2))
    )

    (
      (and 
        (equal lst1 (selection_sort_desc lst1)) 
        (equal lst2 (selection_sort_asc lst2))
      ) 
      (error "Lists must have same orders!")
    )

    (
      (and 
        (equal lst1 (selection_sort_asc lst1)) 
        (equal lst2 (selection_sort_desc lst2))
      ) 
      (error "Lists must have same orders!")
    )
  )  
)

(print (join_sorted_lists_with_order_saving '(9 7 5 3 1) '(8 6 4 2)))
(print (join_sorted_lists_with_order_saving '(1 3 5 7 9) '(2 4 6 8)))

; Error handling
;(print (join_sorted_lists_with_order_saving '(1 3 5 7 9) '(8 6 4 2)))
