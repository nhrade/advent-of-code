def count_overlaps(lines):
    counts = {}
    for line in lines:
        x1, y1, x2, y2 = line

        minx = min(x1, x2)
        maxx = max(x1, x2)
        miny = min(y1, y2)
        maxy = max(y1, y2)

        if x1 == x2:
            # vertical line
            for y in range(miny, maxy + 1):
                if (x1, y) in counts:
                    counts[(x1, y)] += 1
                else:
                    counts[(x1, y)] = 1
        elif y1 == y2:
            # horizontal line
            for x in range(minx, maxx + 1):
                if (x, y1) in counts:
                    counts[(x, y1)] += 1
                else:
                    counts[(x, y1)] = 1
        """else:
            # diagonal
            for i in range(1, maxx - minx):
                if (minx + i, miny + i) in counts:
                    counts[(minx + i, miny + i)] += 1
                else:
                    counts[(minx + i, miny + i)] = 1"""
    overlaps = 0
    for _, count in counts.items():
        if count > 1:
            overlaps += 1
    return overlaps


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
