from typing import OrderedDict
import numpy as np


def create_count_list(fish):
    counts = [0 for _ in range(9)]
    for f in fish:
        counts[f] += 1
    return counts


def run(fish, days):
    counts = create_count_list(fish)
    for _ in range(days):
        for count in range(1, 9):
            amount = counts[count]
            counts[count - 1] += amount
            counts[count] -= amount
        if counts[0] > 0:
            counts[6] += counts[0]
            counts[8] += counts[0]
            counts[0] = 0
    return counts


"""
def run(fish, days):
    num_fish = len(fish)
    fish_arr = create_fish_arr(fish)
    for _ in range(days):
        for i in range(num_fish):
            if fish_arr[i] == 0:
                # add fish
                fish_arr[i] = 6
                fish_arr[num_fish] = 8
                num_fish += 1
            else:
                fish_arr[i] -= 1
    return fish_arr, num_fish


def create_fish_arr(fish):
    fish_arr = np.zeros(10 ** 8, dtype=np.int32)
    fish_arr.fill(-1)
    fish_arr[: len(fish)] = fish
    return fish_arr
"""

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        data = list(map(int, f.read().split(",")))

        counts = run(data.copy(), 80)
        print(f"The number of fish after 80 days is {sum(counts)}")
