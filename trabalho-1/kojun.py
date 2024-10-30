# criar um jogo de kojun. regras:

# Um número no intervalo 1~N deve ser inserido em cada campo de uma área composta por N campos.
# Cada número deve ser usado exatamente uma vez.
# Os números em campos ortogonalmente adjacentes devem ser diferentes.
# Se dois campos na mesma área forem adjacentes verticalmente, o número do campo superior deverá ser maior que o número do campo inferior.

# É parecido com o sudoku, mas com algumas regras diferentes. Precisamos criar um tabuleiro de kojun e implementar um algoritmo que resolva o tabuleiro.

# Não preocupe-se com o desempenho da sua solução, e foque em tamanhos de tabuleiros de até 10x10 no
# primeiro e segundo puzzle, e até 9x9 no terceiro puzzle. Só depois tente melhorar o código para também
# satisfazer tabuleiros maiores. Além disso, implemente a entrada e a saída (resposta do programa) da forma que
# o grupo considerar melhor. A entrada, por exemplo, pode ser fornecida diretamente no código fonte, sem que
# seja necessária a digitação por parte do usuário.
# Dica 1: a técnica de programação mais adequada para resolver este problema é a da “tentativa e erro”
# (backtracking). Pesquise como utilizá-la em Haskell.
# Dica 2: aprenda a jogar o puzzle e depois pense em como modelar o problema (em especial, o tabuleiro
# em si) por meio de alguma estrutura de dados adequada (ex: matriz, árvore, etc). Se preferir, primeiro resolva
# o puzzle em alguma linguagem que o grupo domina (ex: Python, C++, Java).
# Dica 3: a página pode ser traduzida para inglês ou português, para melhor compreensão das regras. Você
# também pode acessar esta página ( https://www.janko.at/Raetsel/Rules.htm ) para entender o que os
# autores dos puzzles querem dizer com algumas palavras, como "area", "orthogonally", "diagonal", "region",
# etc.
# Dica 4: procure como criar um resolvedor do puzzle Sudoku em Haskell, aprenda como funciona e faça as
# adaptações necessárias para resolver o problema proposto para este trabalho

# o tabuleiro nao é exatamente um quadrado, entao precisamos delimitar as areas

def build_board(n):
    board = [[0 for i in range(n)] for j in range(n)]
    print(board)
    
def print_board(board):
    for row in board:
        print(row)

def main():
    n = 6
    build_board(n)  
    
if __name__ == "__main__":
    main()