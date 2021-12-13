import sys


def read_data(file_path: str) -> tuple:
    with open(file_path) as f:
        lines = f.read().split()

    instr = []
    points = {}
    for line in lines:
        if "=" in line:
            instr.append(line)
        if "," in line:
            points[(int(line.split(",")[0]), int(line.split(",")[1]))] = 1

    return points, instr


def print_paper(points: dict):
    rows = max([p[1] for p in points])
    cols = max([p[0] for p in points])

    for row in range(rows + 1):
        for col in range(cols + 1):
            if (col, row) in points:
                print("#", end="")
            else:
                print(".", end="")
        print()


def fold(index: int, horizontal_fold: bool, points: dict) -> dict:
    p_i = int(horizontal_fold)
    d = {}
    for point in points:
        if point[p_i] > index:
            if point[p_i] % index == 0:
                _i = 0
            else:
                _i = index - (point[p_i] % index)

            if horizontal_fold:
                _point = point[0], _i
            else:
                _point = _i, point[1]

            d[_point] = 1
        else:
            d[point] = 1

    return d


def part_one(points: dict, instr: list) -> int:
    horizontal_fold = instr[0][0] == "y"
    index = int(instr[0][2:])
    points = fold(index, horizontal_fold, points)

    return len(points.keys())


def part_two(points: dict, instr: list) -> int:
    for i in instr:
        horizontal_fold = i[0] == "y"
        index = int(i[2:])
        points = fold(index, horizontal_fold, points)

    print_paper(points)

    return 0


def main(file_path: str = "input"):
    points, instr = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(points, instr)}")
    print()
    print("=== Part 2 ===")
    part_two(points, instr)


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
