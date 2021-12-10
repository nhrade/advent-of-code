

def arith_sum(n):
    return n * (n+1) / 2

def calc_fuel_cost(crabs):
    max_crab = max(crabs)
    min_pos = 0
    min_cost = 0
    for crab in crabs:
        min_cost += arith_sum(crab)

    for pos in range(1, max_crab):
        cost = sum(arith_sum(crab - pos) for crab in crabs)
        print(cost)
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
