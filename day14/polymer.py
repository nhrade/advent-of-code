import re
from collections import Counter


def expand(polymer, rules):
    i = 0
    new_polymer = ""
    for i in range(len(polymer)):
        substr = polymer[i : i + 2] if i < (len(polymer) - 1) else polymer[i]
        if substr in rules:
            new_polymer += substr[0] + rules[substr]
        else:
            new_polymer += substr[0]
    return new_polymer


def expand_ntimes(init_polymer, rules, n):
    polymer = init_polymer
    for _ in range(n):
        polymer = expand(polymer, rules)
    return polymer


"""
For part 2 thanks to
https://dev.to/qviper/advent-of-code-2021-python-solution-day-14-4395
"""


def count_expand(polymer, rules, n):
    tmp_poly = Counter(a + b for a, b in zip(polymer, polymer[1:]))
    chars = Counter(polymer)

    for _ in range(n):
        tmp = Counter()
        for (c1, c2), value in tmp_poly.items():
            mc = rules[c1 + c2]
            tmp[c1 + mc] += value
            tmp[mc + c2] += value
            chars[mc] += value
        tmp_poly = tmp
    return max(chars.values()) - min(chars.values())


def least_most_common(string):
    counter = Counter(string)
    common = Counter(string).most_common(len(string))
    return common[0], common[-1]


with open("input.txt") as file:
    contents = file.read()
    contents = contents.split("\n\n")
    polymer = contents[0]
    rules = re.findall("(\w+)\s+->\s+(\w+)", contents[1])
    rules = {k: v for k, v in rules}
    expansion = expand_ntimes(polymer, rules, 10)
    lmc = least_most_common(expansion)
    print(
        f"The difference between most and least common is at 10 steps is {lmc[0][1] - lmc[1][1]}"
    )

    diff = count_expand(polymer, rules, 40)
    print(f"The difference between most and least common is at 40 steps is {diff}")
