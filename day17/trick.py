import re
import os
from dataclasses import dataclass


@dataclass
class Probe:
    velocity: tuple[int, int]
    position: tuple[int, int]


@dataclass
class BBox:
    xmin: int
    xmax: int
    ymin: int
    ymax: int


def find_highesty(ymin: int):
    n = -abs(ymin) - 1
    return n * (n + 1) // 2


def step(probe: Probe):
    vx, vy = probe.velocity
    x, y = probe.position

    probe.position = (
        x + vx,
        y + vy,
    )

    if vx < 0:
        xvel_inc = 1
    elif vx > 0:
        xvel_inc = -1
    else:
        xvel_inc = 0

    probe.velocity = (
        vx + xvel_inc,
        vy - 1,
    )


def in_target(probe: Probe, target: BBox) -> bool:
    return (
        probe.position[0] >= target.xmin
        and probe.position[0] <= target.xmax
        and probe.position[1] >= target.ymin
        and probe.position[1] <= target.ymax
    )


# check if probe exceeds target
def exceeds_target(probe: Probe, target: BBox) -> bool:
    x, y = probe.position
    return x > target.xmax or y < target.ymin


def does_landin_target(probe: Probe, target: BBox) -> bool:
    while not exceeds_target(probe, target):
        if in_target(probe, target):
            return True
        step(probe)
    return False


def count_initial_velocities(target: BBox):
    velocities = [
        (vx, vy)
        for vx in range(2 * abs(target.xmax))
        for vy in range(-abs(target.ymin), abs(target.ymin))
    ]

    count = 0
    for vel in velocities:
        probe = Probe(velocity=vel, position=(0, 0))
        if does_landin_target(probe, target):
            count += 1
    return count


path = os.path.join(os.path.dirname(__file__), "input.txt")

if __name__ == "__main__":
    with open(path, "r") as f:
        contents = f.read()
        [xmin, xmax, ymin, ymax] = [
            int(x)
            for x in re.findall(
                r"x=(-?\d+)\.\.-?(\d+),\s+y=(-?\d+)\.\.(-?\d+)", contents
            )[0]
        ]
        target = BBox(xmin, xmax, ymin, ymax)
        print(f"The highest Y is {find_highesty(target.ymin)}")
        print(f"Number of initial velocities is {count_initial_velocities(target)}")
