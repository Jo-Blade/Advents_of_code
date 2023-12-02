f = open("input", "r")
Lines = f.readlines()
pwd = ""
Dirs = {}
Dirs[""] = []

for line in Lines[1:]:
    if line == "$ cd ..\n":
        pwd = "/".join(pwd.split("/")[:-1])
    elif line[:4] == "$ cd":
        pwd += "/"
        pwd += line[5:-1]
        Dirs[pwd] = [] 
    elif line[:3] == "dir":
        Dirs[pwd].append(pwd + "/" + line[4:-1])
    elif line[0] != "$":
        Dirs[pwd].append(int(line.split(" ")[0]))

Dirs_sizes = {}

def get_size (dir):
    try:
        return Dirs_sizes[dir]
    except:
        s = 0
        for e in Dirs[dir]:
            if isinstance(e, int):
                s += e
            else:
                s += get_size(e)
        Dirs_sizes[dir] = s
        return s

S = 0
for dir in Dirs.keys():
    get_size(dir)

taille_manquante = 30000000 -(70000000 - Dirs_sizes[""])
min_taille = 70000000 
min_dossier = ""

for dir in Dirs_sizes.keys():
    if get_size(dir) >= taille_manquante and get_size(dir) < min_taille:
        min_taille = get_size(dir)
        min_dossier = dir

print(taille_manquante)
print(min_dossier)
print(min_taille)
