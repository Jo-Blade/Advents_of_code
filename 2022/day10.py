f = open("input","r")
L = f.readlines()

x = 1
x_t = []

for l in L:
    if l[:4] == "addx":
        n = int(l[5:-1])
        x_t.append(x)
        x_t.append(x)
        x += n
    elif l[:4] == "noop":
        x_t.append(x)
    else:
        raise "Invalid instruction"

p20 = 20 * x_t[19]
p60 = 60 * x_t[59]
p100 = 100 * x_t[99]
p140 = 140 * x_t[139]
p180 = 180 * x_t[179]
p220 = 220 * x_t[219]

print(p20 + p60 + p100 + p140 + p180 + p220)

for t in range(len(x_t)):
    x = t%40
    s = x_t[t]
    if x == 0:
        print("")

    if (s-1 <= x <= s+1):
        print("#", end="")
    else:
        print(".", end="")
