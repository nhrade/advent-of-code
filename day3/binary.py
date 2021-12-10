# python reimplementation


def btod(binary):
    """
    Converts a binary string to a decimal integer.
    """
    return int(binary, 2)


def common_bit(numbers, bit, most_common=True, tie_breaker="0"):
    """
    Returns the most common bit in a list of numbers.
    """
    count = {}
    for n in numbers:
        if n[bit] not in count:
            count[n[bit]] = 1
        else:
            count[n[bit]] += 1
    if "0" in count and "1" in count and count["0"] == count["1"]:
        return tie_breaker
    return max(count, key=count.get) if most_common else min(count, key=count.get)


def calc_gamma_rate(numbers):
    """
    Calculates the gamma rate of a list of numbers.
    """
    return btod(
        "".join(str(common_bit(numbers, bit)) for bit in range(len(numbers[0])))
    )


def calc_epsilon_rate(numbers):
    """
    Calculates the epsilon rate of a list of numbers.
    """
    return btod(
        "".join(
            str(common_bit(numbers, bit, most_common=False))
            for bit in range(len(numbers[0]))
        )
    )


def calc_power_consumption(numbers):
    """
    Calculates the power consumption of a list of numbers.
    """
    return calc_epsilon_rate(numbers) * calc_gamma_rate(numbers)


def calc_oxygen_rating(numbers):
    """
    Calculates the oxygen rating of a list of numbers.
    """
    remaining = numbers

    for bit in range(len(numbers[0])):
        if len(remaining) > 0:
            cb = common_bit(remaining, bit, tie_breaker="1")
            remaining = list(filter(lambda n: n[bit] == cb, remaining))
    return btod(remaining[-1])


def calc_co2_rating(numbers):
    """
    Calculates the co2 rating of a list of numbers.
    """
    remaining = numbers

    for bit in range(len(numbers[0])):
        if len(remaining) > 0:
            cb = common_bit(remaining, bit, most_common=False)
            remaining = list(filter(lambda n: n[bit] == cb, remaining))
    return btod(remaining[-1])


def life_support_rating(numbers):
    """
    Calculates the life support rating of a list of numbers.
    """
    return calc_co2_rating(numbers) * calc_oxygen_rating(numbers)


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        contents = f.read()
        contents = contents.split("\n")
        gamma_rate = calc_gamma_rate(contents)
        epsilon_rate = calc_epsilon_rate(contents)
        print(f"Gamma rate is {gamma_rate}")
        print(f"Epsilon rate is {epsilon_rate}")
        print(f"Power consumption is {calc_power_consumption(contents)}")

        print(f"Oxygen rating is {calc_oxygen_rating(contents)}")
        print(f"Co2 rating is {calc_co2_rating(contents)}")
        print(f"Life support rating is {life_support_rating(contents)}")
