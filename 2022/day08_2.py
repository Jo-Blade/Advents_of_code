f = open("input","r")
Lines = f.readlines()

# matrice des arbres
M = []

for line in Lines:
    l = []
    l2 = []
    # -1 pour supprimer le caractère de retour à la ligne
    for c in line[:-1]:
        l.append(int(c))
    M.append(l)

# donne le nombre d’arbres visibles à partir d’une liste ordonnée
def nbr_visibles (l,n):
    s = 0
    for e in l:
        if e >= n:
            return s + 1
        else:
            s += 1
    return s

scenic_scores = []
for i in range(1,len(M)-1):
    for j in range(1, len(M[0])-1):
        n = M[i][j]
        # on doit ordonner les listes vers le haut/ gauche
        l_haut = [M[k][j] for k in range(i-1,-1,-1)]
        l_gauche = [M[i][k] for k in range(j-1,-1,-1)]

        l_bas  = [M[k][j] for k in (range(i+1,len(M)))]
        l_droite = [M[i][k] for k in range(j+1,len(M[0]))]

        scenic_scores.append(nbr_visibles(l_haut,n)*nbr_visibles(l_gauche,n)*nbr_visibles(l_bas,n)*nbr_visibles(l_droite,n))

print(max(scenic_scores))
