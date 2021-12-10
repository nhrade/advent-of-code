
def calc_fuel_cost(crabs):
    max_crab = max(crabs)
    min_pos = 0
    min_cost = 0
    costs = {} # dictionary to store costs
    for crab in crabs:
        min_cost += sum(i for i in range(1, crab+1))

    for pos in range(1, max_crab):
        cost = 0
        for crab in crabs:
            s = 0
            if (crab, pos) not in costs or (pos, crab) not in costs:
                s = sum(i for i in range(1, abs(crab - pos) + 1))
                costs[(crab, pos)] = s
            else:
                s = costs[(crab, pos)] if (crab, pos) in costs else costs[(pos, crab)]
            cost += s
        if cost < min_cost:
            min_cost = cost
            min_pos = pos
    return min_pos, min_cost


if __name__ == "__main__":
    with open("input.txt") as file:
        line = next(file)
        data = [int(x) for x in line.split(",")]
        min_pos, min_cost = calc_fuel_cost(data)
        print(f"Minimum cost is {min_cost} with position {min_pos}")
