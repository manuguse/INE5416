;; 2. Crie uma fun¸c˜ao que receba um n´umero x, negativo ou positivo, e retorne seu valor absoluto. Leia x do
;; teclado.

(defun valor-absoluto (x)
    (if (< x 0) ;; se x for menor que 0
        (- x) ;; retorna x negativo
        x)) ;; senão, retorna x

(defun pegar-numero ()
    (format t "Digite um numero: ")
    (read))

(defun main ()
    (let (
        (x (pegar-numero))
    )
    (format t "~a~%" (valor-absoluto x)))
)

(main)