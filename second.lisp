;чтение текстового файла и вывод его содержимого
(defun out (path) 
    (let 
     ((in 
       (open path :if-does-not-exist nil)
     )) 
    (when in 
        (loop for line = (read-line in nil) 
              while line do (format t "~a~%" line)) 
(close in)
        )
     )
    )

(out ("Laba2.txt")) 
