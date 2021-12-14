import os
import string
from collections import Counter


def create_adjacency_list(data):
    lines = data.split("\n")
    adjacency_list = {}
    for line in lines:
        pair = line.split("-")
        if pair[0] not in adjacency_list:
            adjacency_list[pair[0]] = [pair[1]]
            if pair[1] not in adjacency_list:
                adjacency_list[pair[1]] = [pair[0]]
            else:
                adjacency_list[pair[1]].append(pair[0])
        else:
            adjacency_list[pair[0]].append(pair[1])
            if pair[1] not in adjacency_list:
                adjacency_list[pair[1]] = [pair[0]]
            else:
                adjacency_list[pair[1]].append(pair[0])
    return adjacency_list


def is_lower(s):
    return all(c in string.ascii_lowercase for c in s)

def count_lower_gt_max(ls):
    count = Counter(ls)
    for k, v in count.items():
        if is_lower(k) and v > 2:
            return False
    return True

def generate_paths(adjacency_list, paths, start, solution):
    if start == "end":
        paths.append(solution)
    else:
        for v in adjacency_list[start]:
            if v != "start" and ((is_lower(v) and count_lower_gt_max(solution)) or not is_lower(v)):
                generate_paths(adjacency_list, paths, v, solution + [v])

def print_paths(paths):
    for path in paths:
        print(','.join(path))

if __name__ == "__main__":
    path = os.path.join(os.path.dirname(__file__), "input.txt")
    with open(path, "r") as f:
        data = f.read()
        adjacency_list = create_adjacency_list(data)
        paths = []
        generate_paths(adjacency_list, paths, "start", ["start"])
        print_paths(paths)
        print(f"There are {len(paths)} paths")

