import numpy as np


def create_index_dict(board):
    """
    Creates a dictionary from values to indices
    """
    index_dict = {}
    for i, row in enumerate(board):
        for j, elem in enumerate(row):
            if elem not in index_dict:
                index_dict[elem] = (i, j)
    return index_dict


def check_win(row_dict, col_dict):
    """
    Check if a board has won
    """
    row_win = any(x == 5 for x in row_dict.values())
    col_win = any(v == 5 for v in col_dict.values())
    return row_win or col_win


def play_board(board, moves):
    """
    Play a bingo board
    """
    marks = []
    row_dict = {}
    col_dict = {}

    index_dict = create_index_dict(board)
    for move in moves:
        if move in index_dict:
            row, col = index_dict[move]
            marks.append((row, col))
            if row not in row_dict:
                row_dict[row] = 1
            else:
                row_dict[row] += 1
            if col not in col_dict:
                col_dict[col] = 1
            else:
                col_dict[col] += 1
            if check_win(row_dict, col_dict):
                return True, marks, move
    return False, marks, None


def parse_board(board):
    """
    Parse a bingo board into a list of lists
    """
    return np.array([[int(x) for x in line.split()] for line in board.split("\n")])


def play_boards(boards, moves):
    """
    Play all boards in a list
    """
    winners = []
    for board in boards:
        won, marks, move = play_board(board, moves)
        if won:
            winners.append((marks, move))
    return winners


def find_first_winner(boards, moves):
    """
    Find the first winner in a list of boards
    """
    winners = play_boards(boards, moves)
    minarg = np.argmin([len(m) for m, _ in winners])
    return minarg, winners[minarg]


def find_last_winner(boards, moves):
    """
    Find the first winner in a list of boards
    """
    winners = play_boards(boards, moves)
    winners = winners[::-1]
    lengths = [len(m) for m, _ in winners]
    maxarg = np.argmax(lengths)
    return maxarg, winners[maxarg]


def calc_score(board, marks, last_move):
    """
    Calculate the score of a list of boards
    """
    score = 0
    for row in range(board.shape[0]):
        for col in range(board.shape[1]):
            if (row, col) not in marks:
                score += board[row, col]
    score *= last_move
    return score


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        contents = f.read().split("\n\n")
        moves = [int(x) for x in contents[0].split(",")]
        boards = [parse_board(x) for x in contents[1:]]
        index, (marks, last_move) = find_first_winner(boards, moves)
        score = calc_score(boards[index], marks, last_move)
        print(f"Score of first winning board is {score}")

        index, (marks, last_move) = find_last_winner(boards, moves)
        score = calc_score(boards[index], marks, last_move)
        print(f"Score of last winning board is {score}")
