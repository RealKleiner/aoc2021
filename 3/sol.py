import sys


def read_file(file_name: str) -> list:
    with open(file_name) as f:
        return f.read().split()


def count_ones_zeroes(data: list, index: int) -> tuple:
    ones = zeroes = 0
    for line in data:
        if line[index] == "1":
            ones += 1
        else:
            zeroes += 1
    return zeroes, ones


def calc_new_oxygen_list(data: list, index: int) -> list:
    out = []
    zeroes, ones = count_ones_zeroes(data, index)
    for line in data:
        if ones >= zeroes:
            if line[index] == "1":
                out.append(line)
        else:
            if line[index] == "0":
                out.append(line)
    return out


def calc_new_scrubber_list(data: list, index: int) -> list:
    out = []
    zeroes, ones = count_ones_zeroes(data, index)
    for line in data:
        if ones >= zeroes:
            if line[index] == "0":
                out.append(line)
        else:
            if line[index] == "1":
                out.append(line)
    return out


def main(file_name: str = "input"):
    data = read_file(file_name)
    oxygen = data.copy()
    scrubber = data.copy()

    for i in range(len(data[0])):
        if len(oxygen) > 1:
            oxygen = calc_new_oxygen_list(oxygen, i)
        if len(scrubber) > 1:
            scrubber = calc_new_scrubber_list(scrubber, i)

    print(f"oxy: {oxygen}\nscru:{scrubber}\n")
    print(f"{oxygen[0]} {scrubber[0]}")

    oxygen = int(oxygen[0], 2)
    scrubber = int(scrubber[0], 2)
    print(f"{oxygen} {scrubber}")

    print(f"Result: {oxygen * scrubber}")


if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
