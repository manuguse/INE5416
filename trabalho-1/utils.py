class board1:
    def get_board(self):
        board = [['██' for _ in range(6)] for _ in range(6)]
        board[0][0] = 2
        board[0][4] = 1
        board[1][3] = 3
        board[2][1] = 3
        board[2][4] = 5
        board[2][5] = 3
        board[4][2] = 3
        board[4][4] = 4
        board[4][5] = 2
        return board

    def get_blocks(self):
        return [
            [1, 1, 2, 2, 2, 3],
            [4, 4, 4, 4, 4, 3],
            [5, 6, 6, 6, 4, 7],
            [5, 5, 5, 6, 7, 7],
            [8, 8, 9, 10, 10, 10],
            [11, 11, 9, 9, 10, 10]
        ]
        
    def get_tamanho(self):
        return len(self.get_board())
        
class board2:
    def get_board(self):
        
        board = [['██' for _ in range(8)] for _ in range(8)]
        
        board[1][1] = 1
        board[1][2] = 3
        board[3][2] = 3
        board[4][1] = 5
        board[4][3] = 3
        board[5][0] = 2
        board[6][6] = 3
        board[7][2] = 5
        board[7][3] = 3
        
        return board

    def get_blocks(self): 
        return [   
            [1, 1, 2, 2, 3, 4, 5, 5],
            [1, 1, 6, 2, 8, 4, 4, 5],
            [6, 6, 6, 7, 8, 9, 10, 10],
            [19, 19, 19, 7, 8, 9, 9, 10],
            [11, 7, 7, 7, 7, 9, 9, 10],
            [11, 12, 13, 13, 13, 14, 15, 10],
            [12, 12, 12, 12, 17, 14, 14, 14],
            [16, 17, 17, 17, 17, 14, 18, 18]
        ]

    def get_tamanho(self):
        return len(self.get_board())