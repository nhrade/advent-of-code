def get_min_max(x1, y1, x2, y2):
    return min(x1, x2), max(x1, x2), min(y1, y2), max(y1, y2)


def add_point(counts, point):
    if point in counts:
        counts[point] += 1
    else:
        counts[point] = 1


def count_straight(counts, x1, y1, x2, y2):
    xmin, xmax, ymin, ymax = get_min_max(x1, y1, x2, y2)

    if x1 == x2:
        # vertical line
        for y in range(ymin, ymax + 1):
            add_point(counts, (x1, y))
    elif y1 == y2:
        # horizontal line
        for x in range(xmin, xmax + 1):
            add_point(counts, (x, y1))


def count_diagonals(counts, x1, y1, x2, y2):
    # diagonals with a bit of calculus
    # (xmin, ymax) -> (xmax, ymin)
    # (xmin, ymin) ->  (xmin, ymax)
    xmin, xmax, ymin, ymax = get_min_max(x1, y1, x2, y2)

    dy = y2 - y1
    dx = x2 - x1
    if abs(dx) > 0:
        slope = dy / dx
        if slope == -1:
            for i in range(ymax - ymin):
                add_point(counts, (xmin + i, ymax - i))
        elif slope == 1:
            for i in range(ymax - ymin):
                add_point(counts, (xmin + i, ymin + i))


def count_overlaps(lines):
    counts = {}
    for line in lines:
        x1, y1, x2, y2 = line
        count_straight(counts, x1, y1, x2, y2)
        count_diagonals(counts, x1, y1, x2, y2)
    return sum(count > 1 for _, count in counts.items())


def parse_input(contents):
    lines = []
    for line in contents:
        line = line.split("->")
        nums = []
        for pair in line:
            pair = pair.split(",")
            try:
                nums.append(int(pair[0].strip()))
                nums.append(int(pair[1].strip()))
            except ValueError:
                print("Error:", pair)
        lines.append((nums[0], nums[1], nums[2], nums[3]))
    return lines


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        data = f.read().strip()
        contents = data.split("\n")
        lines = parse_input(contents)
        overlaps = count_overlaps(lines)
        print("Overlaps:", overlaps)
