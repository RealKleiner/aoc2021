import sys


class Location:
    def __init__(self, value: int, index: tuple):
        self.value = value
        self.x, self.y = index
        self.visited = False

    def get_neighbours(self, x_max: int, y_max: int) -> list:
        out = []

        if self.x - 1 >= 0:
            out.append((self.x - 1, self.y))
        if self.x + 1 < x_max:
            out.append((self.x + 1, self.y))
        if self.y - 1 >= 0:
            out.append((self.x, self.y - 1))
        if self.y + 1 < y_max:
            out.append((self.x, self.y + 1))

        return out

    def visit(self):
        self.visited = True

    def __lt__(self, other) -> bool:
        return self.value < other.value

    def __eq__(self, other) -> bool:
        return self.value == other.value

    def __gt__(self, other) -> bool:
        return not (self == other and self < other)

    def __str__(self) -> str:
        return f"({self.x}, {self.y})"


class Cave:
    def __init__(self, data: list):
        self.cave = {}
        self.x_max = len(data[0])
        self.y_max = len(data)

        for y, row in enumerate(data):
            for x, cell in enumerate(row):
                self.cave[(x, y)] = Location(cell, (x, y))

    def get_lowest_points(self) -> list:
        out = []
        for key, item in self.cave.items():
            neighbours = [
                self.cave[loc] for loc in item.get_neighbours(self.x_max, self.y_max)
            ]
            if item < min(neighbours):
                out.append(item)
        return out

    def get_basin(self, loc: Location) -> list:
        if loc.value > 8:
            return []

        out = [loc]
        loc.visit()
        for _loc in loc.get_neighbours(self.x_max, self.y_max):
            if (
                not self.cave[_loc].visited
                and self.cave[_loc].value > loc.value
                and self.cave[_loc].value != 9
            ):
                out += self.get_basin(self.cave[_loc])

        return out


def calc_point_values(locs: list) -> int:
    return sum([loc.value for loc in locs]) + len(locs)


def read_data(file_path: str) -> Cave:
    with open(file_path) as f:
        points = [[int(x) for x in line] for line in f.read().split("\n")][:-1]
        return Cave(points)


def part_one(data: Cave) -> int:
    points = data.get_lowest_points()
    return calc_point_values(points)


def part_two(data: Cave) -> int:
    lowest = data.get_lowest_points()
    basins = []
    for loc in lowest:
        basins.append(data.get_basin(loc))

    basins = sorted(basins, key=len)
    return len(basins[-1]) * len(basins[-2]) * len(basins[-3])


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
