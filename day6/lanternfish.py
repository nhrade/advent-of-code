from collections import Counter


def run(fish, days):
    counts = Counter(fish)
    timers = Counter({timer: 0 for timer in range(10)})
    counts.update(timers)

    for day in range(days):
        counts[7] += counts.get(0, 0)
        counts[9] += counts.get(0, 0)
        counts = Counter({count: counts.get(count + 1, 0) for count in counts})

    return counts


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        data = list(map(int, f.read().split(",")))

        fish_counts = run(data, 256)
        total = sum(fish_counts.values())
        print(f"The number of fish after 80 days is {total}")
