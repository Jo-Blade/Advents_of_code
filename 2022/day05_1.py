f = open("input")
Lines = f.readlines()

N = 9
T = []

for i in range(N):
    T.append([])

i = 0
line = Lines[i]

while line[1] != "1":
    for j in range(N):
        if line[1 + 4*j] != " ":
            T[j].append(line[1 + 4*j])
    i += 1
    line = Lines[i]
i += 2

for j in range(N):
    T[j].reverse()

def move(n, a, b):
    for i in range(n):
        u = T[a].pop()
        T[b].append(u)


while i < len(Lines):
    spl = Lines[i].split(" ")
    move(int(spl[1]), int(spl[3]) - 1, int(spl[5]) - 1)
    i += 1

str = ""
for j in range(N):
    str += T[j][-1]

print(str)
