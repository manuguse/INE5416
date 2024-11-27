from termcolor import colored
import utils

# Constantes e inicialização
COLORS = ['red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'light_grey', 'dark_grey', 'light_red', 
          'light_green', 'light_yellow', 'light_blue', 'light_magenta', 'light_cyan']

# Função de inicialização do jogo
def initialize_game():
    board_instance = utils.board2()
    return board_instance.get_tamanho(), board_instance.get_blocks(), board_instance.get_board()

TAMANHO, BLOCKS, BOARD = initialize_game()

#####################################
######## Funções de Get/Set #########
#####################################

def get_board_position(row, col):
    return BOARD[row][col]

def set_board_position(row, col, value):
    BOARD[row][col] = value

def get_block_position(row, col):
    return BLOCKS[row][col]

def get_region_cells(region_id):
    """Obtém todas as células de uma região específica."""
    return [(i, j) for i in range(TAMANHO) for j in range(TAMANHO) if get_block_position(i, j) == region_id]

def get_region_size(row, col):
    region_id = get_block_position(row, col)
    return sum(1 for i in range(TAMANHO) for j in range(TAMANHO) if get_block_position(i, j) == region_id)

#####################################
######## Funções de Exibição ########
#####################################
import os
def print_board():
    """Imprime o tabuleiro com cores de acordo com os blocos."""
    for i in range(TAMANHO):
        for j in range(TAMANHO):
            color = COLORS[(get_block_position(i, j) - 1) % len(COLORS)]
            cell_value = get_board_position(i, j)
            print(colored(cell_value, color), end=" "if cell_value != '██' else "")
        print()
    os.system('clear')
    

#####################################
######## Funções de Validação #######
#####################################

def is_valid(row, col, num):
    """Verifica se um número pode ser colocado na célula (row, col) sem violar as regras."""
    region_id = get_block_position(row, col)
    
    # Verificar duplicidade no bloco
    for i, j in get_region_cells(region_id):
        if get_board_position(i, j) == num:
            return False

    # Verificação ortogonal
    for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        r, c = row + dr, col + dc
        if 0 <= r < TAMANHO and 0 <= c < TAMANHO and get_board_position(r, c) == num:
            return False

    # Verificação de grandeza vertical dentro da mesma região
    if row > 0 and get_block_position(row - 1, col) == region_id:
        if get_board_position(row - 1, col) != '██' and get_board_position(row - 1, col) <= num:
            return False
    elif row < TAMANHO - 1 and get_block_position(row + 1, col) == region_id:  # Célula abaixo na mesma região
        if get_board_position(row + 1, col) != '██' and get_board_position(row + 1, col) >= num:
            return False

    return True

#####################################
######## Funções de Solução #########
#####################################

def get_possible_values(row, col):
    """Retorna valores possíveis para a célula (row, col) com base na região."""
    region_size = get_region_size(row, col)
    region_id = get_block_position(row, col)
    used_values = {get_board_position(i, j) for i, j in get_region_cells(region_id) if get_board_position(i, j) != '██'}
    return list(reversed([num for num in range(1, region_size + 1) if num not in used_values]))

def solve():
    """Resolve o quebra-cabeça usando permutações e backtracking."""
    empty_cells = [(i, j) for i in range(TAMANHO) for j in range(TAMANHO) if get_board_position(i, j) == '██']
    if not empty_cells:
        return True

    # Seleciona a primeira célula vazia e tenta os valores possíveis
    row, col = empty_cells[0]
    possible_values = get_possible_values(row, col)

    # Tenta cada valor possível para a célula
    for num in possible_values:
        if is_valid(row, col, num):
            set_board_position(row, col, num)
            print_board()
            if solve():
                return True
            set_board_position(row, col, '██')  # Backtrack

    return False

#####################################
############ Execução ###############
#####################################

print("Tabuleiro inicial:")
print_board()
if solve():
    print("Solução encontrada:")
else:
    print("Não foi possível encontrar uma solução.")
print_board()
