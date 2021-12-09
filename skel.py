import sys


def read_data(file_path: str) -> list:
    with open(file_path) as f:
        return f.read().split()


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
