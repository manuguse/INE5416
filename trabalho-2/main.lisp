;; Definicoes de Estruturas e Tipos

(defstruct puzzle
  board   ; Tabuleiro do puzzle: matriz de numeros ou NIL
  blocks  ; Blocos do puzzle: matriz de identificadores de blocos
  size)   ; Tamanho do tabuleiro

;; Funcoes Auxiliares para Manipulacao do Tabuleiro e Blocos

(defun initialize-puzzle (board blocks)
  "Inicializa o puzzle com o tabuleiro e os blocos fornecidos."
  (make-puzzle :board board :blocks blocks :size (length board)))

(defun get-board-position (board row col)
  "Obtem o valor de uma celula no tabuleiro. Retorna NIL se estiver vazia."
  (nth col (nth row board)))

(defun set-board-position (board row col value)
  (let ((new-board (copy-seq board)))
    (setf (nth col (nth row new-board)) value)
    new-board))


(defun get-block-id (blocks row col)
  "Obtem o ID do bloco para uma determinada celula."
  (nth col (nth row blocks)))

(defun get-region-cells (blocks region-id)
  "Retorna as coordenadas (r, c) das celulas pertencentes a uma regiao especifica."
  (loop for r from 0 below (length blocks)
        append (loop for c from 0 below (length (first blocks))
                     when (= (get-block-id blocks r c) region-id)
                     collect (cons r c))))

(defun get-orthogonal-neighbors (board row col)
  "Retorna as coordenadas dos vizinhos ortogonais (acima, abaixo, esquerda, direita)."
  (remove-if-not
    (lambda (rc)
      (and (>= (car rc) 0) (< (car rc) (length board))
           (>= (cdr rc) 0) (< (cdr rc) (length (first board)))))
    `((, (+ row 1) . ,col)
      (, (- row 1) . ,col)
      (, row . ,(+ col 1))
      (, row . ,(- col 1)))))

(defun get-adjacent-regions (board blocks region-id)
  "Retorna uma lista de IDs de regioes adjacentes a uma determinada regiao."
  (let ((region-cells (get-region-cells blocks region-id)))
    (remove-if (lambda (x) (= x region-id))
               (remove-duplicates
                 (mapcan (lambda (rc)
                           (mapcar (lambda (neighbor)
                                     (get-block-id blocks (car neighbor) (cdr neighbor)))
                                   (get-orthogonal-neighbors board (car rc) (cdr rc))))
                         region-cells)))))

(defun get-region-size (blocks region-id)
  "Retorna o tamanho de uma regiao especifica."
  (length (get-region-cells blocks region-id)))

;; Funcoes de Validacao

(defun is-valid (puzzle row col num)
  "Verifica se e valido colocar `num` na celula (row, col) do puzzle."
  (let* ((board (puzzle-board puzzle))
         (blocks (puzzle-blocks puzzle))
         (region-id (get-block-id blocks row col))
         (region-cells (get-region-cells blocks region-id))
         (region-numbers (remove-if #'null
                                    (mapcar (lambda (rc)
                                              (get-board-position board (car rc) (cdr rc)))
                                            region-cells)))
         (row-numbers (remove-if #'null (nth row board)))
         (col-numbers (remove-if #'null (mapcar (lambda (r) (get-board-position board r col))
                                               (loop for r from 0 below (puzzle-size puzzle)
                                                     collect r))))
         (block-rule (not (member num region-numbers)))
         (vertical-block-rule
          (every (lambda (r)
                   (let ((existing (get-board-position board r col)))
                     (or (null existing)
                         (and (< r row)
                              (< existing num))
                         (and (> r row)
                              (> existing num))))
                   )
                 (mapcar #'car region-cells)))
         (neighbors (get-orthogonal-neighbors board row col))
         (neighbor-rule
          (every (lambda (rc)
                   (let ((val (get-board-position board (car rc) (cdr rc))))
                     (not (and val (= val num))))
                   )
                 neighbors)))
    (and (not (member num row-numbers))
         (not (member num col-numbers))
         block-rule
         vertical-block-rule
         neighbor-rule)))

;; Funcao de Resolucao (Backtracking)

(defun solve-puzzle (puzzle)
  "Resolve o puzzle usando backtracking. Retorna o tabuleiro resolvido ou NIL se nao houver solucao."
  (labels ((solve-rec (puzzle)
           (let* ((board (puzzle-board puzzle))
                  (size (puzzle-size puzzle))
                  (empty-cell
                   (loop for r from 0 below size
                         do (loop for c from 0 below size
                                  if (null (get-board-position board r c))
                                  return (cons r c)))))
             (if (null empty-cell)
                 ;; Nenhuma célula vazia, puzzle resolvido
                 board
                 (let* ((row (car empty-cell))
                        (col (cdr empty-cell))
                        (blocks (puzzle-blocks puzzle))
                        (region-id (get-block-id blocks row col))
                        (region-size (get-region-size blocks region-id))
                        (region-cells (get-region-cells blocks region-id))
                        (region-numbers (remove-if #'null
                                                   (mapcar (lambda (rc)
                                                             (get-board-position board (car rc) (cdr rc)))
                                                           region-cells)))
                        (valid-numbers (remove-if (lambda (n)
                                                    (member n region-numbers))
                                                  (loop for n from 1 to region-size collect n))))
                   (format t "Tentando preencher célula (~A, ~A) da região ~A~%" row col region-id)
                   (loop for num in valid-numbers
                         when (is-valid puzzle row col num)
                         do (let* ((new-board (set-board-position board row col num))
                                   (new-puzzle (make-puzzle :board new-board :blocks blocks :size size)))
                              (format t "Colocando ~A em (~A, ~A)~%" num row col)
                              (let ((solution (solve-rec new-puzzle)))
                                (when solution
                                  (return solution))))
                         finally
                           (format t "Nenhuma solução válida para (~A, ~A)~%" row col)))))))
  (solve-rec puzzle)))


;; Funcao para Imprimir o Tabuleiro

(defun print-board (board)
  "Imprime o tabuleiro de forma legivel."
  (dolist (row board)
    (format t "~{~A~^ ~}~%" (mapcar (lambda (cell) (if cell cell ".")) row))))

;; Exemplos de Puzzles

;; Exemplo 1
(defparameter *board1*
  '((2 NIL NIL NIL 1 NIL)
    (NIL NIL NIL 3 NIL NIL)
    (NIL 3 NIL NIL 5 3)
    (NIL NIL NIL NIL NIL NIL)
    (NIL NIL 3 NIL 4 2)
    (NIL NIL NIL NIL NIL NIL)))

(defparameter *blocks1*
  '((1 1 2 2 2 3)
    (4 4 4 4 4 3)
    (5 6 6 6 4 7)
    (5 5 5 6 7 7)
    (8 8 9 10 10 10)
    (11 11 9 9 10 10)))

;; Exemplo 2
(defparameter *board2*
  '((NIL NIL)
    (NIL NIL)))

(defparameter *blocks2*
  '((1 1)
    (2 2)))

;; Funcao Principal para Testar o Puzzle

(defun main ()
  "Funcao principal para testar a resolucao do puzzle."
       (let* ((puzzle (initialize-puzzle *board1* *blocks1*)))
         (format t "Tabuleiro inicial:~%")
         (print-board (puzzle-board puzzle))
         (let ((solution (solve-puzzle puzzle)))
           (if solution
               (progn
                 (format t "~%Solucao encontrada:~%" "")
                 (print-board solution))
               (format t "~%Nao foi possivel encontrar uma solucao.~%")))))

(main)
