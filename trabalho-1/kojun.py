from termcolor import colored

class Kojun:
    def __init__(self, tamanho) -> None:
        self.tamanho = tamanho
        self.board = [['██' for _ in range(tamanho)] for _ in range(tamanho)]
        self.colors = ['red', 'green', 'blue', 'yellow', 'magenta', 'cyan', 'dark_grey']
       

        # Definir blocos com posições específicas
        self.blocks = [
            [(0, 0), (0, 1), (1, 1), (1, 2), (2, 2), (2, 1), (1,0)],  # Bloco 1
            [(0, 2), (0, 3), (0, 4), (1, 3), (1, 4)],  # Bloco 2
            [(0, 5), (1, 5), (2, 5), (2, 4), (2, 3), (1, 1)],  # Bloco 3
            [(3, 1), (4, 1), (4, 2), (5, 2)],  # Bloco 4
            [(3, 2), (3, 3), (3, 4), (4, 3), (4, 4), (5, 3)],  # Bloco 5
            [(3, 5), (4, 5), (5, 5), (5, 4), (5, 3)],  # Bloco 6
            [(2,0), (3, 0), (4, 0), (4, 1), (5, 1), (5, 0)]  # Bloco 7
            ]
        
        self.board[0][0] = '1'
        self.board[0][1] = '2'
        self.board[0][2] = '3'
        self.board[0][3] = '4'	
        self.board[0][4] = '5'

        self.print_board()

    def print_board(self):
        for i in range(self.tamanho):
            for j in range(self.tamanho):
                color = self.get_block_color((i, j))
                print(colored(self.board[i][j], color), end=('' if self.board[i][j] == '██' else ' '))
            print()
    
    def get_block_color(self, pos):
        for i, block in enumerate(self.blocks):
            if pos in block:
                return self.colors[i]
            
    def solve_kojun(self):
        pass

# Criação do tabuleiro de tamanho 6x6
k = Kojun(6)
print(k.solve_kojun())