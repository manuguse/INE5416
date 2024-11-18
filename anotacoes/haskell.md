# HASKELL

## O que é Haskell?

Haskell é uma linguagem de programação funcional pura, criada em 1990 por um grupo de pesquisadores liderado por Simon Peyton Jones. A linguagem é baseada no cálculo lambda e possui um sistema de tipos forte e estático, que permite a inferência de tipos. Haskell é conhecida por sua elegância e expressividade, além de promover o uso de funções de alta ordem, imutabilidade e recursão.

## Características

- **Sintaxe**: a sintaxe de Haskell é baseada em expressões, que são compostas por funções e operadores. A aplicação de funções é feita de forma prefixada, ou seja, o nome da função é seguido pelos seus argumentos, separados por espaços. Por exemplo, a expressão `f x y` representa a aplicação da função `f` aos argumentos `x` e `y`.
- **Funções de alta ordem**: Haskell suporta funções de alta ordem, ou seja, funções que podem receber outras funções como argumentos e/ou retornar funções como resultados. Isso permite a composição de funções de forma elegante e concisa.
- **Imutabilidade**: em Haskell, as variáveis são imutáveis, ou seja, uma vez que um valor é atribuído a uma variável, ele não pode ser alterado. Isso evita efeitos colaterais e facilita o raciocínio sobre o código.
- **Recursão**: a recursão é uma técnica muito comum em Haskell, já que a linguagem não possui estruturas de repetição tradicionais, como o for e o while. A recursão é utilizada para percorrer listas, calcular fatoriais, entre outras tarefas.

## Exemplos

### Hello, World!

```haskell
main = putStrLn "Hello, World!"
```

### Fatorial

```haskell
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
```

### Fibonacci

```haskell
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

### Ler input do usuário

```haskell
main = do
  putStrLn "Digite um número: "
  n <- readLn
  print n
```

## Referências

- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](https://learnyouahaskell.com/chapters)
- [Real World Haskell](http://book.realworldhaskell.org/)
