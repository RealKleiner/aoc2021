import sys
from enum import IntEnum


class Point:
    def __init__(self, *args):
        if len(args) == 1:
            self.x = int(args[0].split(",")[0])
            self.y = int(args[0].split(",")[1])
        elif len(args) == 2:
            self.x = args[0]
            self.y = args[1]
        else:
            self.x = 0
            self.y = 0
        self.counted = False

    def __iter__(self):
        yield self.x
        yield self.y

    def __hash__(self):
        return hash(tuple(self))

    def __lt__(self, other) -> bool:
        return self.x < other.x or (self.x == other.x and self.y < other.y)

    def __eq__(self, other) -> bool:
        return self.x == other.x and self.y == other.y

    def __gt__(self, other) -> bool:
        return not (self == other or self < other)

    def __str__(self) -> str:
        return f"({self.x}, {self.y})"


class Orientation(IntEnum):
    HORIZONTAL = 0
    VERTICAL = 1
    DIAGONAL = 2


class Line:
    def __init__(self, p1: Point, p2: Point):
        self.start = min(p1, p2)
        self.end = max(p1, p2)

        if p1.x == p2.x:
            self.orientation = Orientation.HORIZONTAL
            self.points = [
                Point(self.start.x, y)
                for y in range(self.start.y, self.end.y + 1)  # noqa: E501
            ]

        elif p1.y == p2.y:
            self.orientation = Orientation.VERTICAL
            self.points = [
                Point(x, self.start.y)
                for x in range(self.start.x, self.end.x + 1)  # noqa: E501
            ]
        else:
            self.orientation = Orientation.DIAGONAL
            self.points = [
                Point(x, y)
                for x, y in zip(
                    range(self.start.x, self.end.x + 1),
                    range(self.start.y, self.end.y + 1),  # noqa: e501
                )
            ]
            if len(self.points) == 0:
                x_start = min(self.start.x, self.end.x)
                y_start = max(self.start.y, self.end.y)
                x_end = max(self.start.x, self.end.x)
                y_end = min(self.start.y, self.end.y)

                self.points = [
                    Point(x, y)
                    for x, y in zip(
                        range(x_start, x_end + 1),
                        range(y_start, y_end - 1, -1),  # noqa: E501
                    )
                ]

    def __str__(self) -> str:
        out = f"{self.start} -> {self.end}: "
        return out + ", ".join([str(p) for p in self.points])

    def count_overlap(self, orient: tuple, store: dict):
        if self.orientation in orient:
            for p in self.points:
                if p not in store:
                    store[p] = 0
                store[p] += 1


def read_data(file_path: str) -> list:
    with open(file_path) as f:
        lines = f.read().split()
        out = []
        for i in range(0, len(lines), 3):
            out.append(Line(Point(lines[i]), Point(lines[i + 2])))

        return out


def part_one(data: list) -> int:
    store = {}
    count = 0

    for l1 in data:
        l1.count_overlap((Orientation.HORIZONTAL, Orientation.VERTICAL), store)

    for key, value in store.items():
        if value > 1:
            count += 1

    return count


def part_two(data: list) -> int:
    store = {}
    count = 0

    for l1 in data:
        l1.count_overlap((Orientation), store)

    for key, value in store.items():
        if value > 1:
            count += 1

    return count


def main(file_path: str = "input"):
    data = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(data)}")
    print()
    print("=== Part 2 ===")
    print(f"Result: {part_two(data)}")


if __name__ == "__main__":
    if len(sys.argv) >= 2:
        main(sys.argv[1])
    else:
        main()
