from termcolor import colored
import utils

colors = ['red', 'green', 'yellow', 'blue', 'magenta', 'cyan',
          'light_grey', 'dark_grey', 'light_red', 'light_green', 
          'light_yellow', 'light_blue', 'light_magenta', 'light_cyan']

tamanho = utils.get_tamanho()
blocks = utils.get_blocks()
board = utils.get_board()

def print_board(): # funciona
    """Imprime o tabuleiro com cores de acordo com os blocos."""
    for i in range(tamanho):
        for j in range(tamanho):
            color = get_block_color(i, j)
            cell_value = board[i][j]
            print(colored(cell_value, color), end=('' if cell_value == '██' else ' '))
        print()
    print()

def get_block_color(row, col): # funciona
    """Retorna a cor correspondente ao ID do bloco na posição (row, col)."""
    block_id = blocks[row][col] - 1
    return colors[block_id % len(colors)]

def get_region_size(row, col):
    region_id = blocks[row][col]
    return sum(1 for i in range(tamanho) for j in range(tamanho) if blocks[i][j] == region_id)

def is_valid(row, col, num):
    """Verifica se um número pode ser colocado na célula (row, col) sem violar as regras."""
    region_id = blocks[row][col]

    # Verificar se o número já está na mesma região
    for i in range(tamanho):
        for j in range(tamanho):
            if blocks[i][j] == region_id and (board[i][j]) == num:
                return False

    # Verificar células adjacentes ortogonalmente
    for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        r, c = row + dr, col + dc
        if 0 <= r < tamanho and 0 <= c < tamanho and board[r][c] == num:
            return False

    # Verificar a regra de ordem vertical para a mesma região
    if row > 0 and blocks[row][col] == blocks[row - 1][col]:
        if board[row - 1][col] != '██' and (board[row - 1][col]) <= num:
            return False
    if row < tamanho - 1 and blocks[row][col] == blocks[row + 1][col]:
        if board[row + 1][col] != '██' and (board[row + 1][col]) >= num:
            return False

    return True


def get_candidate_cells():
    """Ordena as células vazias para serem preenchidas primeiro as que estão em regiões menores e com mais restrições."""
    empty_cells = [(i, j) for i in range(tamanho) for j in range(tamanho) if board[i][j] == '██']
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
        board[row][col] = num
        if solve():
            return True
        board[row][col] = '██' 

    return False
    
def semi_solved(): # método para ver melhor o print, não é necessário
    """Verifica se pelo menos metade das células estão preenchidas."""
    count = sum(1 for i in range(tamanho) for j in range(tamanho) if board[i][j] != '██')
    return count >= tamanho * tamanho // 2

print_board()
solve()
print_board()
