(defconstant Spisok (list 1 1 1 1 0 1 0 0 0 0 0 0 1 1))

; Сжатие списка
(defun compress (x) ; compress - выполняет сжатие списка
    (if (consp x)
        (compr (car x) 1 (cdr x))
        x)
    )

(defun compr (elt n lst) ; compr - рекурсивная функция, elt - последний встреченный элемент, lst - остаток списка, подлежащий дальнейшей компрессии
    (if (null lst)
        (list (n-elts elt n))
        (let ((next (car lst))) ; car - первый элемент в списке, nth - возвращает n-нный элемент списка.
            (if (eq next elt) ; eq -  equals
                (compr elt (+ n 1) (cdr lst)) ; cdr - хвост списка
                (cons (n-elts elt n) ; cons (n-elts elt n) - конкатенация списков, в результате получится новый список, состоящий из элементов n-elts, elt n
                    (compr next 1 (cdr lst)
                           )
                      )
                )
             )
        )
    )

(defun n-elts (elt n) ; n-elts - возвращающая сжатое представление n элементов elt
    (if (> n 1)
        (list n elt)
        elt)
    )

; Разжатие списка
(defun decompress (lst) ; decompress - возвращает список к нормальному виду
    (if (null lst)
        nil
        (let ((elt (car lst))
                (rest (decompress (cdr lst))))
            (if (consp elt) ; функция (consp arg) возвращает true, если arg имеет тип cons, в противном случае возвращает false.
                (append (apply #'list-of elt)rest) ; Append - добавляет в конец существующего файла #' -  синтаксическая обертка для function.
                (cons elt rest)
                )
             )
        )
    )

(defun list-of (n elt) ; list-of - копирование атома и раскрытие списков
    (if (zerop n)
        nil
        (cons elt (list-of (- n 1) elt)
              )
        )
    )

(print (compress Spisok)) ; Вывод сжатия списка
(print (decompress (compress Spisok))) ; Вывод разжатия списка
