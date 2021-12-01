def make_int_list(seq: list) -> list:
    return [int(x) for x in seq]


with open("input") as f:
    lines = f.read()

lines = lines.split()
last_window = sum(make_int_list(lines[0:3]))

start = 1
end = 4
increases = 0

while len(lines) >= end:
    curr_wind = sum(make_int_list(lines[start:end]))
    if curr_wind > last_window:
        increases += 1

    last_window = curr_wind
    start += 1
    end += 1

print(f"There were {increases} increases in depth.")
