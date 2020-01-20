(defconstant L (list 1 4 2 9 8 6 3 7))


; Вставка
(defun IN (A N LST)
   (cond ((NULL LST) (cons A LST))
      ((eq N 1) (cons A LST))
      (T (cons (car LST)
            (IN A (- N 1)
               (CDR LST))))))
                            
; Удаление
(defun DEL (N LST)
   (COND ((EQ N 1) (CDR LST))
      (T (CONS (CAR LST)
            (DEL (- N 1) (CDR LST))))))
            
; Поиск
(defun SCH (N list)
                   (cond((eq list nil) "Элемент не найден")
                        ((eq (car list) N) "Элемент найден")
                        (t (SCH N (cdr list)))))
                        
; Печать списка                        
(print "list ")(print L)

(print "Вставка числа 10 на 4-е место: ")
(set 'spis (IN 10 4 L))
(print spis)

(print "Удаление из списка 3 номера: ")
(set 'spis (DEL 3 L))
(print spis)

(print "Поиск элемента по значению: ")
(print "Значение 5")
(print (SCH 5 L))
(print "Значение 8")
(print (SCH 8 L))
