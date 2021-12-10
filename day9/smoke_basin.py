import numpy as np
from functools import reduce

def read_matrix(input: str) -> np.ndarray:
    return np.array([[x for x in row] for row in input.split('\n')], dtype=np.int32)


def get_neighbors(matrix: np.ndarray, r: int, c: int) -> [(int, int)]:
    neighbors = []
    if r > 0:
        neighbors.append((r-1, c))
    if r < matrix.shape[0]-1:
        neighbors.append((r+1, c))
    if c > 0:
        neighbors.append((r, c-1))
    if c < matrix.shape[1]-1:
        neighbors.append((r, c+1))
    return neighbors


def find_low_points(heightmap: np.ndarray) -> [(int, int)]:
    low_points = []
    for r in range(heightmap.shape[0]):
        for c in range(heightmap.shape[1]):
            neighbors = get_neighbors(heightmap, r, c)
            values = np.array([heightmap[r, c] for r, c in neighbors], dtype=np.int32)
            if np.all(heightmap[r, c] < values):
                low_points.append((r, c))
    return low_points


def bfs(heightmap: np.ndarray, r: int, c: int) -> set[(int, int)]:
    queue = []
    seen = set()
    queue.append((r, c))
    while len(queue) > 0:
        curr = queue.pop(0)
        seen.add(curr)
        neighbors = get_neighbors(heightmap, curr[0], curr[1])
        for r, c in neighbors:
            h = heightmap[r, c]
            if (r, c) not in seen and h != 9 and h > heightmap[curr[0], curr[1]]:
                queue.append((r, c))
    return seen


def total_risk_level(heightmap: np.ndarray, low_points: [(int, int)]):
    return sum([heightmap[point[0], point[1]]+1 for point in low_points])

def find_basins(heightmap: np.ndarray, low_points: [(int, int)]):
    total_size = 1
    basins = []
    for low_point in low_points:
        basin = bfs(heightmap, low_point[0], low_point[1])
        basins.append(basin)
        total_size += len(basin)
    return basins, total_size

with open('input.txt') as file:
    data = file.read()
    heightmap = read_matrix(data)
    low_points = find_low_points(heightmap)
    risk = total_risk_level(heightmap, low_points)
    print(f"Risk level: {risk}")

    basins, total_size = find_basins(heightmap, low_points)
    basin_lengths = sorted([len(basin) for basin in basins], reverse=True)
    size = reduce(lambda acc, s: acc * s, basin_lengths[:3], 1)
    print(f"Product basin sizes: {size}")




