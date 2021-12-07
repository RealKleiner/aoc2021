import sys


def create_board(data: list) -> list:
    if not data:
        return []

    board = []
    for row in data:
        _row = row.split()
        _row = [(int(x), False) for x in _row]
        board.append(_row)

    return board


def create_boards(data: list) -> tuple:
    if not data:
        return []

    board = create_board(data[0:5])
    return [board] + create_boards(data[6:])


def bingo(board: list) -> bool:
    # check row
    for row in board:
        count = 0
        for cell in row:
            count += 1 if cell[1] else 0

        if count >= 5:
            return True

    # check col
    for i in range(len(board[0])):
        count = 0
        for j in range(len(board[0])):
            count += 1 if board[j][i][1] else 0

        if count >= 5:
            return True

    return False


def mark_number(board: list, number: int):
    for row in board:
        for index, cell in enumerate(row):
            if cell[0] == number:
                row[index] = (cell[0], True)
                return


def calc_score(board: list, number: int) -> int:
    score = 0
    for row in board:
        for cell in row:
            score += cell[0] if not cell[1] else 0

    return score * number


def read_data(file_path: str) -> tuple:
    numbers_to_draw = []
    boards = []
    with open(file_path) as f:
        numbers_to_draw = [int(x) for x in f.readline().split(",")]
        read_boards = f.readlines()
        boards = create_boards(read_boards[1:])

    return numbers_to_draw, boards


def remove_board(boards: list, board: list):
    for i in range(len(boards)):
        if boards[i] == board:
            del boards[i]
            return


def part_one(numbers_to_draw: list, boards: list) -> int:
    for number in numbers_to_draw:
        for board in boards:
            mark_number(board, number)
            if bingo(board):
                return calc_score(board, number)


def part_two(numbers_to_draw: list, boards: list) -> int:
    for number in numbers_to_draw:
        for board in boards:
            mark_number(board, number)

        for index, board in enumerate(boards):
            if bingo(board):
                if len(boards) > 1:
                    del boards[index]
                else:
                    return calc_score(board, number)


def main(file_path: str = "input"):
    numbers, boards = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(numbers, boards)}")
    print()
    print("=== Part 2 ===")
    print(f"Result: {part_two(numbers, boards)}")


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
