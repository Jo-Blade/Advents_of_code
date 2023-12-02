f = open("input", "r")
L = f.readlines()
list = []

for line in L:
    h = len(line) // 2
    lg = line[0:h]
    ld = line[h:len(line)]
    for c in lg:
        if ld.__contains__(c):
            if 'a' <= c <= 'z':
                list.append(ord(c) - ord('a') + 1)
                break
            else:
                list.append(ord(c) - ord('A') + 27)
                break
print(sum(list))
