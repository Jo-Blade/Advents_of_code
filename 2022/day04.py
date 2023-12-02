f = open("input", "r")
Lines = f.readlines()

s = 0
for line in Lines:
    l = line.split(',')
    l1 = l[0].split('-')
    l2 = l[1].split('-')
    if int(l1[0]) <= int(l2[0]) <= int(l1[1]):
        s += 1
    elif int(l1[0]) <= int(l2[1]) <= int(l1[1]):
        s += 1
    elif int(l2[0]) <= int(l1[0]) <= int(l2[1]):
        s += 1
    elif int(l2[0]) <= int(l1[1]) <= int(l2[1]):
        s += 1

print(s)
