from functools import reduce

score_table = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137
}

completion_table = {
    ')': 1,
    ']': 2,
    '}': 3,
    '>': 4
}

opening_tag = {
    ')': '(',
    ']': '[',
    '>': '<',
    '}': '{'
}

closing_tag = {
    '(': ')',
    '[': ']',
    '<': '>',
    '{': '}'
}


def check_errors(line: str) -> (str, int):
    """ Returns character and index of first character that is incorrect, returns -1 and None if correct """
    stack = []
    for i, ch in enumerate(line):
        if ch  in '([{<':
            stack.append(ch)
        else:
            if len(stack) == 0 or stack[-1] != opening_tag[ch]:
                return ch, i
            stack.pop()
    return None, -1


def complete_line(line: str) -> str:
    stack = []
    for i, ch in enumerate(line):
        if ch in '([{<':
            stack.append(ch)
        else:
            if len(stack) != 0 and stack[-1] == opening_tag[ch]:
                stack.pop()
    return ''.join(closing_tag[ch] for ch in stack[::-1])

def error_score(contents: [str]) -> int:
    score = 0
    for line in contents:
        ch, pos = check_errors(line)
        if pos != -1:
            score += score_table[ch]
    return score


def autocomplete_score(contents: [str]) -> int:
    scores = []
    for line in contents:
        if (check_errors(line)[1]) == -1:
            complete = complete_line(line)
            scores.append(reduce(lambda acc, ch: acc * 5 + completion_table[ch], complete, 0))
    scores = sorted(scores)
    return scores[int(len(scores)/2)]


if __name__ == '__main__':
    with open('input.txt') as file:
        data = file.read()
        contents = data.split('\n')
        score = error_score(contents)
        print(f"Error score is {score}")

        score = autocomplete_score(contents)
        print(f"Autocomplete score is {score}")