# LISP

## O que é?
Lisp (LISt Processing) é uma das linguagens de programação mais antigas, criada em 1958, com foco em processamento de listas e programação simbólica. É uma linguagem funcional e fortemente baseada no uso de expressões prefixadas, ou seja, o operador aparece antes dos operandos, como em (+ 2 3).

## Características
- **Sintaxe**: a sintaxe de Lisp é baseada em listas, que são representadas por parênteses. Cada lista é composta por um operador e seus operandos, separados por espaços. Por exemplo, a expressão (+ 2 3) representa a soma de 2 e 3.
- **Funções**: em Lisp, funções são tratadas como cidadãos de primeira classe, ou seja, podem ser passadas como argumentos para outras funções, retornadas como valores e armazenadas em variáveis.
- **Recursão**: a recursão é uma técnica muito comum em Lisp, já que a linguagem não possui estruturas de repetição tradicionais, como o for e o while.
- **Macros**: Lisp possui um sistema de macros poderoso, que permite a definição de novas formas de controle de fluxo e de estruturas de dados.
- **Interpretação**: a maioria dos dialetos de Lisp são interpretados, o que permite uma interação mais dinâmica com o código.

## Exemplos
### Hello, World!
```lisp
(print "Hello, World!")
```

### Fatorial
```lisp
(defun fatorial (n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))
```

### Fibonacci
```lisp
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```

### Ler input do usuário
```lisp
(defun ler-input ()
  (format t "Digite um número: ")
  (finish-output)
  (read))
```
