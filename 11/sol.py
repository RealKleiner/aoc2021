import sys


class Octopus:
    def __init__(self, energy: int):
        self.energy = energy
        self.flashed = False

    def flash(self) -> bool:
        if self.energy > 9:
            self.energy = 0
            self.flashed = True
            return True
        return False

    def energy_increase(self):
        if not self.flashed:
            self.energy += 1

    def ready(self):
        self.flashed = False

    def __str__(self) -> str:
        return f"{self.energy} - {self.flashed}"


def read_data(file_path: str) -> list:
    with open(file_path) as f:
        return [[Octopus(int(c)) for c in line] for line in f.read().split()]


def find_neighbours(index: tuple, x_max: int, y_max: int) -> list:
    out = []
    for y in range(index[1] - 1, index[1] + 2):
        if 0 <= y < y_max:
            for x in range(index[0] - 1, index[0] + 2):
                if (x, y) == index:
                    continue
                if 0 <= x < x_max:
                    out.append((x, y))

    return out


def increase_value(data: list):
    for row in data:
        for cell in row:
            cell.energy_increase()


def handle_flash(data: list):
    flashed = True
    x_max = len(data[0])
    y_max = len(data)

    while flashed:
        flashed = False
        for y, row in enumerate(data):
            for x, cell in enumerate(row):
                if cell.flash():
                    flashed = True
                    for neighbour in find_neighbours((x, y), x_max, y_max):
                        octopus = data[neighbour[1]][neighbour[0]]
                        octopus.energy_increase()


def count_flashes(data: list) -> int:
    count = 0
    for y, row in enumerate(data):
        for x, cell in enumerate(row):
            if cell.flashed:
                count += 1
            cell.ready()

    return count


def sim_step(data: list) -> int:
    increase_value(data)
    handle_flash(data)
    return count_flashes(data)


def part_one(data: list) -> int:
    count = 0
    for i in range(100):
        count += sim_step(data)

    return count


def part_two(data: list) -> int:
    target = len(data) * len(data[0])
    flashes = 0

    steps = 0
    while target != flashes:
        # print(flashes)
        flashes = sim_step(data)
        steps += 1

    return steps


def main(file_path: str = "input"):
    data = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(data)}")
    print()
    data = read_data(file_path)
    print("=== Part 2 ===")
    print(f"Result: {part_two(data)}")


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
