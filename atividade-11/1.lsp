;; 1. Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne x
;; y
;; . Leia x e y do teclado.

(defun potencia (x y) 
  (expt x y))

(defun pegar-numero ()
  (format t "Digite um numero: ")
  (read))

(defun main ()
    (let (
        (x (pegar-numero))
        (y (pegar-numero))
    )
    (format t "~a~%" (potencia x y))))

(main)