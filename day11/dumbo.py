import numpy as np
from typing import List
import os


def get_neighbors(matrix, r, c):
    neighbors = [
        (r - 1, c),
        (r + 1, c),
        (r, c - 1),
        (r, c + 1),
        (r - 1, c - 1),
        (r - 1, c + 1),
        (r + 1, c - 1),
        (r + 1, c + 1),
    ]
    return [
        (r, c)
        for r, c in neighbors
        if (0 <= r < matrix.shape[0]) and (0 <= c < matrix.shape[1])
    ]


def parse_input(data):
    return np.array([[int(x) for x in row] for row in data.split("\n")], dtype=np.int32)


def step(grid):
    grid += 1
    flashgrid = np.full(grid.shape, False, dtype=bool)
    count = 0
    while len(flashed := np.argwhere(grid > 9)) > 0:
        flashset = {(pos[0], pos[1]) for pos in flashed}
        for r, c in flashset:
            neighbors = get_neighbors(grid, r, c)
            for n in neighbors:
                if n not in flashset:
                    grid[n] += 1
            flashgrid[r, c] = True
            grid[flashgrid] = 0
            count += 1
    return count


def run(grid, num_steps):
    total = 0
    for _ in range(num_steps):
        count = step(grid)
        total += count
    return total


def find_first_flash(grid):
    i = 1
    history = [grid.copy()]
    while True:
        count = step(grid)
        if count == (grid.shape[0] * grid.shape[1]):
            return i, history
        i += 1
        history.append(grid.copy())


path = os.path.join(os.path.dirname(__file__), "input.txt")
with open(path) as file:
    data = parse_input(file.read())
    data2 = data.copy()
    count = run(data, 100)
    print(f"There have been {count} octopus flashed")

    first_flash, history = find_first_flash(data2)
    print(f"The first step where all octopuses flashed was at step {first_flash}")
