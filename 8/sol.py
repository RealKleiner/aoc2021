import sys


class Data:
    def __init__(self, line: str):
        self.output = [
            "".join(sorted(val)) for val in line.split("|")[1].split()
        ]  # noqa: E501
        self.signals = [
            "".join(sorted(val)) for val in line.split("|")[0].split()
        ] + self.output
        self.int_to_signal = {key: "" for key in range(10)}
        self.signal_to_int = {}
        self.segment_to_signal = {}  # normal signal to scramble
        self.signal_to_segment = {}  # scrambled signal to normal

    def __str__(self) -> str:
        out = f'Signals: {", ".join([val for val in self.signals])}\n'
        out += f'Output: {", ".join([val for val in self.output])}\n'
        return out

    def count_unique_output(self) -> int:
        count = 0
        for output in self.output:
            if len(output) in [2, 3, 4, 7]:
                count += 1
        return count

    def determine_unqiue_numbers(self):
        for val in self.signals:
            length = len(val)
            if length == 2:
                self.int_to_signal[1] = val
                self.signal_to_int[val] = 1
            elif length == 3:
                self.int_to_signal[7] = val
                self.signal_to_int[val] = 7
            elif length == 4:
                self.int_to_signal[4] = val
                self.signal_to_int[val] = 4
            elif length == 7:
                self.int_to_signal[8] = val
                self.signal_to_int[val] = 8

    def _get_signal_combos(self, numbers: list) -> str:
        out = ""
        for number in numbers:
            for c in self.int_to_signal[number]:
                if c not in out:
                    out += c
        return out

    def determine_A(self):
        for c in self.int_to_signal[7]:
            if c not in self.int_to_signal[1]:
                self.signal_to_segment[c] = "a"
                self.segment_to_signal["a"] = c
                return

    def determine_B(self):
        for c in self.int_to_signal[4]:
            if c not in self.int_to_signal[3]:
                self.segment_to_signal["b"] = c
                self.signal_to_segment[c] = "b"
                return

    def determine_D(self):
        exclude = self.segment_to_signal["b"] + self.int_to_signal[1]
        for c in self.int_to_signal[4]:
            if c not in exclude:
                self.segment_to_signal["d"] = c
                self.signal_to_segment[c] = "d"
                return

    def _check_match(self, curr: str, target: str) -> bool:
        count = 0
        for c in curr:
            if c in target:
                count += 1
        return count

    def determine_9(self):
        signals = self._get_signal_combos([1, 4, 7])

        for val in self.signals:
            if len(val) != 6:
                continue
            if self._check_match(val, signals) == 5:
                self.signal_to_int[val] = 9
                self.int_to_signal[9] = val
                return

    def determine_3(self):
        signals = self._get_signal_combos([1, 7])

        for val in self.signals:
            if len(val) != 5:
                continue
            if self._check_match(val, signals) == 3:
                self.signal_to_int[val] = 3
                self.int_to_signal[3] = val
                return

    def determine_0(self):
        eight = self.int_to_signal[8]
        d_seg = self.segment_to_signal["d"]
        out = ""
        for c in eight:
            if c == d_seg:
                continue
            out += c

        self.signal_to_int[out] = 0
        self.int_to_signal[0] = out

    def determine_6(self):
        for signal in self.signals:
            if len(signal) != 6:
                continue
            if signal in [self.int_to_signal[0], self.int_to_signal[9]]:
                continue
            self.int_to_signal[6] = signal
            self.signal_to_int[signal] = 6
            return

    def determine_C_F(self):
        one = self.int_to_signal[1]
        six = self.int_to_signal[6]

        for c in one:
            if c not in six:
                self.segment_to_signal["c"] = c
                self.signal_to_segment[c] = "c"
                break
        for _c in one:
            if _c != c:
                self.segment_to_signal["f"] = _c
                self.signal_to_segment[_c] = "f"
                return

    def determine_2_5(self):
        eight = self.int_to_signal[8]
        exclude = self.segment_to_signal["f"] + self.segment_to_signal["b"]
        signal = ""
        for c in eight:
            if c not in exclude:
                signal += c

        self.signal_to_int[signal] = 2
        self.int_to_signal[2] = signal

        for _signal in self.signals:
            if len(_signal) != 5:
                continue
            if _signal not in [signal, self.int_to_signal[3]]:
                self.int_to_signal[5] = _signal
                self.signal_to_int[_signal] = 5
                return

    def determine_everything(self):
        self.determine_unqiue_numbers()
        self.determine_3()
        self.determine_9()
        self.determine_A()
        self.determine_B()
        self.determine_D()
        self.determine_0()
        self.determine_6()
        self.determine_C_F()
        self.determine_2_5()

    def convert_output(self) -> int:
        out = ""
        # print(self.signal_to_int)
        for signal in self.output:
            out += str(self.signal_to_int[signal])
        return int(out)


def read_data(file_path: str) -> list:
    with open(file_path) as f:
        lines = f.read().split("\n")
        return [Data(line) for line in lines[:-1]]


def part_one(data: list) -> int:
    count = 0
    for d in data:
        count += d.count_unique_output()
    return count


def part_two(data: list) -> int:
    res = 0
    for d in data:
        d.determine_everything()
        res += d.convert_output()
    return res


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
