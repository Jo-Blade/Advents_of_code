f = open("input","r")
Lines = f.readlines()

# matrice des arbres
M = []
# matrices des tailles maximales pour ne pas etre visible
V = []

for line in Lines:
    l = []
    l2 = []
    # -1 pour supprimer le caractère de retour à la ligne
    for c in line[:-1]:
        l.append(int(c))
        l2.append(-1) # on initialise V a la matrice -1
                      #car les arbres ont des tailles >= 0
    M.append(l)
    V.append(l2)

# on met a jour V, on ne parcourt pas les bords, c’est inutile
for i in range(1,len(M)-1):
    for j in range(1, len(M[0])-1):
        haut = max([M[k][j] for k in range(i)])
        bas  = max([M[k][j] for k in (range(i+1,len(M)))])
        gauche = max([M[i][k] for k in range(j)])
        droite = max([M[i][k] for k in range(j+1,len(M[0]))])

        V[i][j] = min(haut, bas, gauche, droite)

# enfin on compte les arbres visibles depuis l’exterieur
s = 0
for i in range(len(M)):
    for j in range(len(M[0])):
        if M[i][j] > V[i][j]:
            s += 1

print(s)
