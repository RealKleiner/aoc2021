import sys


def read_data(file_path: str) -> list:
    with open(file_path) as f:
        return [[int(x) for x in line] for line in f.read().split("\n")[:-1]]


def get_neighbours(index: tuple, data: list) -> list:
    y, x = index

    if y == 0 and x == 0:
        return [data[0][1], data[1][0]]
    elif y == 0 and x == len(list[0]) - 1:
        return [data[0][-2], data[1][-1]]
    elif y == len(data) - 1 and x == 0:
        return [data[-1][1], data[-2][0]]
    elif y == len(data) - 1 and x == len(data[0]) - 1:
        return [data[-1][-2], data[-2][-1]]

    if y == 0:
        return [data[y][x - 1], data[y][x + 1], data[y + 1][x]]
    elif y == len(data) - 1:
        return [data[y][x - 1], data[y][x + 1], data[y - 1][x]]

    if x == 0:
        return [data[y][x + 1], data[y + 1][x], data[y - 1][x]]
    elif x == len(data[0]) - 1:
        return [data[y][x - 1], data[y + 1][x], data[y - 1][x]]

    return [data[y + 1][x], data[y - 1][x], data[y][x + 1], data[y][x - 1]]


def check_if_lowest(loc: int, neighbours: list) -> bool:
    return loc < min(neighbours)


def part_one(data: list) -> int:
    return 0


def part_two(data: list) -> int:
    return 0


def main(file_path: str = "input"):
    data = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(data)}")
    print()
    print("=== Part 2 ===")
    print(f"Result: {part_two(data)}")


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
