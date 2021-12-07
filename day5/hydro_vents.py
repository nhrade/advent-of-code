import requests
from dataclasses import dataclass


@dataclass
class Line:
    x1: int
    y1: int
    x2: int
    y2: int

    def is_vertical(self):
        return self.x1 == self.x2

    def is_horizontal(self):
        return self.y1 == self.y2

    def is_parallel(self, other):
        """Check if self and other lines are parallel"""
        return (
            self.is_vertical()
            and other.is_vertical()
            or self.is_horizontal()
            and other.is_horizontal()
        )

    def count_overlaps(self, other):
        """Check how many times self and other line intersect"""
        if self.is_vertical():
            if self.y1 <= other.y1 <= self.y2:
                return 1
        elif self.is_horizontal():
            if self.x1 <= other.x1 <= self.x2:
                return 1
        return 0


def count_overlapping_lines(lines):
    overlaps = 0
    seen = set()
    for i, line1 in enumerate(lines):
        for j, line2 in enumerate(lines):
            if i != j and (
                (line1.is_vertical() and line2.is_horizontal())
                or (line1.is_horizontal() and line2.is_vertical())
            ):
                overlaps += line1.count_overlaps(line2)
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
        lines.append(Line(*nums))
    return lines


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        data = f.read().strip()
        contents = data.split("\n")
        lines = parse_input(contents)
        num_overlaps = count_overlapping_lines(lines)
        print(f"number of overlapping {num_overlaps}")
