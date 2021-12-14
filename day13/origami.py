import os
import re

def print_grid(points, foldx=None, foldy=None):
    xmax = max(x for x in [x for x, _ in points])
    ymax = max(y for y in [y for _, y in points])
    for y in range(ymax+1):
        if foldy is not None and foldy == y:
            print('- ' * (xmax+1))
        else:
            for x in range(xmax+1):
                if foldx is not None and foldx == x:
                    print('|', end=' ')
                elif (x, y) in points:
                    print('#', end=' ')
                else:
                    print('.', end=' ')
            print()


def fold(points, ins):
    new_points = []
    if ins[0] == 'x':
        foldx = ins[1]
        for x, y in points:
            if x > foldx:
                new_point = (2*foldx - x, y)
                new_points.append(new_point)
                new_points = list(set(new_points))
            else:
                new_points.append((x, y))
    elif ins[0] == 'y':
        foldy = ins[1]
        for x, y in points:
            if y > foldy:
                new_point = (x, 2*foldy - y)
                new_points.append(new_point)
                new_points = list(set(new_points))
            else:
                new_points.append((x, y))
    return new_points


def decode_contents(points, instructions):
    for ins in instructions:
        points = fold(points, ins)
    print_grid(points)

def parse_input(contents):
    contents = contents.split("\n\n")
    points = [tuple([int(x) for x in line.strip().split(',')])
              for line in contents[0].split('\n')]
    occur = re.findall(r"fold along (x|y)=(\d+)", contents[1])
    instructions = [(var, int(num)) for var, num in occur]
    return points, instructions


path = os.path.join(os.path.dirname(__file__), "input.txt")
if __name__ == '__main__':
    with open(path) as file:
        contents = file.read()
        points, instructions = parse_input(contents)
        new_points = fold(points, instructions[0])
        print(f"Number of points after first fold is {len(new_points)}")
        decode_contents(points, instructions)



