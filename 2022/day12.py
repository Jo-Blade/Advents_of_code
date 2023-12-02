f = open("input", "r")
Lines = f.readlines()

M = []

start = (0,0)
end = (0,0)
x, y = 0, 0

for line in Lines:
    l = []
    y = 0
    for c in line[:-1]:
        if c == 'S':
            start = (x,y)
            l.append('a')
        elif c == 'E':
            end = (x,y)
            l.append('z')
        else:
            l.append(c)
        y += 1
    x += 1
    M.append(l)

dists = [[2**31 for j in range(len(M[0]))] for i in range(len(M))]

pile = []

def Depile(t):
    x = t[0]
    y = t[1]
    z = t[2]
    d = t[3]

    if (0 <= x < len(M)) and (0 <= y < len(M[0])):
        if ((ord(M[x][y]) - ord(z)) <= 1) and (dists[x][y] > d):
            dists[x][y] = d
            # parcours en largeur a de meilleures performances
            # qu’en profondeur, donc insert au lieu de append
            pile.insert(0, (x+1,y  , M[x][y], d+1))
            pile.insert(0, (x-1,y  , M[x][y], d+1))
            pile.insert(0, (x  ,y+1, M[x][y], d+1))
            pile.insert(0, (x  ,y-1, M[x][y], d+1))
        else:
            return
    else:
        return

# changer la valeur pour résoudre la partie 1
part = 2
if part == 1:
    # on initialise avec la position 'S'
    pile.append((start[0], start[1], 'a', 0))
else:
    # on initialise avec toutes les positions d’élévation 'a'
    for x in range(len(M)):
        for y in range(len(M[0])):
            if M[x][y] == 'a':
                pile.append((x,y,'a',0))

while len(pile):
    Depile(pile.pop())

print(dists[end[0]][end[1]])
