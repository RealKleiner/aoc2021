import sys

STATES = 8


def create_empty_dict() -> dict:
    return {key: 0 for key in range(STATES + 1)}


def read_data(file_path: str) -> dict:
    out = create_empty_dict()
    with open(file_path) as f:
        for fish in f.read().split(","):
            out[int(fish)] += 1

    return out


def sim_day(data: dict) -> dict:
    _d = create_empty_dict()
    for key, val in data.items():
        if key == 0:
            _d[8] += val
            _d[6] += val
        else:
            _d[key - 1] += val
    return _d


def run_sim(data: dict, days: int) -> int:
    for i in range(days):
        data = sim_day(data)

    return sum(data.values())


def part_one(data: dict) -> int:
    return run_sim(data, 80)


def part_two(data: dict) -> int:
    return run_sim(data, 256)


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
