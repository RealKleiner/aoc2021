import sys
import math


def read_data(file_path: str) -> dict:
    out = {}
    with open(file_path) as f:
        for crab in f.read().split(","):
            crab = int(crab)
            if crab not in out.keys():
                out[crab] = 0
            out[crab] += 1

    return out


def get_endpoints(data: dict) -> tuple:
    return min(data.keys()), max(data.keys())


def part_one(data: dict) -> int:
    start, end = get_endpoints(data)
    fuel = math.inf

    for pos in range(start, end + 1):
        _fuel = 0
        for key, value in data.items():
            _fuel += abs(key - pos) * value

        if _fuel < fuel:
            fuel = _fuel

    return fuel


def calc_fuel(start: int, end: int) -> int:
    res = 0
    for cost, step in enumerate(range(start, end), 1):
        res += cost

    return res


def part_two(data: dict) -> int:
    start, end = get_endpoints(data)
    fuel = math.inf

    for pos in range(start, end + 1):
        _fuel = 0
        for key, value in data.items():
            _fuel += calc_fuel(min(key, pos), max(key, pos)) * value

        if _fuel < fuel:
            fuel = _fuel

    return fuel


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
