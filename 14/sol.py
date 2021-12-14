import sys


def read_data(file_path: str) -> tuple:
    rules = {}
    with open(file_path) as f:
        data = f.read().split()

    for i in range(1, len(data[1:]), 3):
        rules[data[i]] = data[i + 2]

    polymer = {}
    for i in range(len(data[0])):
        key = "".join(data[0][i : i + 2])
        if key not in polymer:
            polymer[key] = 0
        polymer[key] += 1

    return polymer, rules


def step(polymer: dict, rules: dict) -> dict:
    _d = {}
    for key, val in polymer.items():
        if len(key) < 2:
            _d[key] = polymer[key]
            continue
        left_key = key[0] + rules[key]
        right_key = rules[key] + key[1]

        if left_key not in _d:
            _d[left_key] = 0
        if right_key not in _d:
            _d[right_key] = 0

        _d[left_key] += val
        _d[right_key] += val
    return _d


def count_occurences(polymer: dict) -> tuple:
    _d = {}
    for key in polymer:
        if key[0] not in _d:
            _d[key[0]] = 0
        _d[key[0]] += polymer[key]
    return max(_d.values()), min(_d.values())


def part_one(polymer: dict, rules: dict) -> int:
    for i in range(10):
        polymer = step(polymer, rules)
    a, b = count_occurences(polymer)
    return a - b


def part_two(polymer: dict, rules: dict) -> int:
    for i in range(40):
        polymer = step(polymer, rules)
    a, b = count_occurences(polymer)
    return a - b


def main(file_path: str = "input"):
    polymer, rules = read_data(file_path)
    print("=== Part 1 ===")
    print(f"Result: {part_one(polymer, rules)}")
    print()
    print("=== Part 2 ===")
    print(f"Result: {part_two(polymer, rules)}")


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
