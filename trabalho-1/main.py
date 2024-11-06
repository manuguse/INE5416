from termcolor import colored
import utils

# Lista de cores para os blocos do tabuleiro
COLORS = ['red', 'green', 'yellow', 'blue', 'magenta', 'cyan',
          'light_grey', 'dark_grey', 'light_red', 'light_green', 
          'light_yellow', 'light_blue', 'light_magenta', 'light_cyan']

# Inicialização do tabuleiro, blocos e tamanho
def initialize_game():
    board_instance = utils.board1()
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

def get_region_size(row, col):
    region_id = get_block_position(row, col)
    return sum(1 for i in range(TAMANHO) for j in range(TAMANHO) if get_block_position(i, j) == region_id)

#####################################
######## Funções de Exibição ########
#####################################

def get_block_color(row, col):
    """Retorna a cor correspondente ao ID do bloco na posição (row, col)."""
    block_id = get_block_position(row, col) - 1
    return COLORS[block_id % len(COLORS)]

def print_element(cell_value, color):
    print(colored(cell_value, color), end=('' if cell_value == '██' else ' '))

def print_board():
    """Imprime o tabuleiro com cores de acordo com os blocos."""
    for i in range(TAMANHO):
        for j in range(TAMANHO):
            color = get_block_color(i, j)
            cell_value = get_board_position(i, j)
            print_element(cell_value, color)
        print()
    print()

#####################################
######## Funções de Validação #######
#####################################

def same_region(region_id, num):
    """Verifica se o número já existe na mesma região."""
    return any(get_block_position(i, j) == region_id and get_board_position(i, j) == num
               for i in range(TAMANHO) for j in range(TAMANHO))

def ortogonally_valid(row, col, num):
    """Verifica a validade ortogonal da posição."""
    for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        r, c = row + dr, col + dc
        if 0 <= r < TAMANHO and 0 <= c < TAMANHO and get_board_position(r, c) == num:
            return False
    return True

def vertically_valid(row, col, num):
    """Verifica a validade vertical da posição."""
    if row > 0 and get_board_position(row - 1, col) == num:
        return False
    if row < TAMANHO - 1 and get_board_position(row + 1, col) == num:
        return False
    return True

def is_valid(row, col, num):
    """Verifica se um número pode ser colocado na célula (row, col) sem violar as regras."""
    region_id = get_block_position(row, col)
    return not same_region(region_id, num) and ortogonally_valid(row, col, num) and vertically_valid(row, col, num)

#####################################
######## Funções de Solução #########
#####################################

def get_candidate_cells():
    """Ordena as células vazias para priorizar as de regiões menores e com mais restrições."""
    empty_cells = [(i, j) for i in range(TAMANHO) for j in range(TAMANHO) if get_board_position(i, j) == '██']
    return sorted(empty_cells, key=lambda cell: (get_region_size(cell[0], cell[1]), len(get_valid_numbers(cell[0], cell[1]))))

def get_valid_numbers(row, col):
    """Retorna uma lista de números válidos para uma célula específica."""
    region_size = get_region_size(row, col)
    return [num for num in range(1, region_size + 1) if is_valid(row, col, num)]

def solve():
    """Resolve o quebra-cabeça usando backtracking otimizado."""
    candidate_cells = get_candidate_cells()
    if not candidate_cells:
        return True

    row, col = candidate_cells[0]
    for num in get_valid_numbers(row, col):
        set_board_position(row, col, num)
        if solve():
            return True
        set_board_position(row, col, '██')

    return False

#####################################
############ Execução ###############
#####################################

print_board()
if solve():
    print("Solução encontrada:")
else:
    print("Não foi possível encontrar uma solução.")
print_board()
