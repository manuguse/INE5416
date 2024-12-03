# resolvedor de um kojun com scala

para instalar o scala, faça

```bash
sudo apt install scala
```

garanta estar na pasta `trabalho-2/`
garanta a existência da pasta `out/`

para compilar o programa, faça:

```bash
scalac -d out/ src/*.scala
```

para rodar com todos os tabuleiros disponíveis, execute:

```bash
scala -cp out/ Main
```

para rodar com um tabuleiro específico, execute:

```bash
scala -cp out/ Main <tabuleiro>
```

se preferir, execute o script, com os seguintes passos:

dê permissão de execução ao script:

```bash
chmod +x ./run.sh
```

execute o script, de forma que se desejar um tabuleiro específico, passa o número como argumento, e use o argumento v/f para compilar ou não o programa:

roda o tabuleiro específico:

```bash
./run.sh <tabuleiro> <v/f>
```

roda todos:

```bash
./run.sh <v/f>
```
