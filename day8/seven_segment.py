

if __name__ == '__main__':
    with open('input.txt') as file:
        data = file.read()
        lines = data.split('\n')
        contents = [[seg.strip().split(' ') for seg in line.split('|')] for line in lines]
        print(contents)