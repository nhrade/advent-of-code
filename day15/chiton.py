import numpy as np
from heapq import heappush, heappop
from dataclasses import dataclass, field
import os


@dataclass(order=True)
class PosItem:
    priority: int
    pos: tuple[int, int] = field(compare=False)


path = os.path.join(os.path.dirname(__file__), "input.txt")


def find_path(arr):
    pq = []
    visited = set()
    cost = np.zeros_like(arr, dtype=np.int32)
    cost.fill(2 ** 31 - 1)
    prev = np.zeros(shape=(cost.shape[0], cost.shape[1], 2), dtype=np.int32)
    cost[0, 0] = 0
    pq.append(PosItem(0, (0, 0)))

    while pq:
        item = heappop(pq)
        r, c = item.pos
        visited.add((r, c))

        if (
            (r + 1, c) not in visited
            and r < arr.shape[0] - 1
            and cost[r, c] + arr[r + 1, c] < cost[r + 1, c]
        ):
            cost[r + 1, c] = cost[r, c] + arr[r + 1, c]
            prev[r + 1, c, :] = [r, c]
            heappush(pq, PosItem(cost[r + 1, c], (r + 1, c)))
        if (
            (r, c + 1) not in visited
            and c < arr.shape[1] - 1
            and cost[r, c] + arr[r, c + 1] < cost[r, c + 1]
        ):
            cost[r, c + 1] = cost[r, c] + arr[r, c + 1]
            prev[r, c + 1, :] = [r, c]
            heappush(pq, PosItem(cost[r, c + 1], (r, c + 1)))
    return prev, cost


if __name__ == "__main__":
    with open(path) as file:
        contents = file.read()
        arr = np.asarray(
            [[int(n) for n in line] for line in contents.split("\n")], dtype=np.int32
        )

        prev, cost = find_path(arr)
        print(f"Lowest cost path is {cost[cost.shape[0]-1, cost.shape[1]-1]}")
