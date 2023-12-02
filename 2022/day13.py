f = open("input","r")
Lines = f.readlines()

def str2list (str):
    l = []
    i = 0
    while str[i] != ']':
        if str[i] == '[':
            l2, j = str2list(str[i+1:])
            l.append(l2)
            i += j + 2
        elif '0' <= str[i] <= '9':
            j = i
            while('0' <= str[j] <= '9'):
                j += 1
            l.append(int(str[i:j]))
            i = j
        else:
            i += 1
    return l, i

L = []

for line in Lines:
    if line != "\n":
        L.append(str2list(line[1:])[0])

def estEntier(e):
    return type(e) == type(1)

def compare(l1,l2):
    if len(l1) + len(l2) == 0:
        raise "error"
    elif len(l1) == 0:
        return True
    elif len(l2) == 0:
        return False
    else:
        a, b = l1[0], l2[0]
        if estEntier(a) and estEntier(b):
            if a == b:
                return compare(l1[1:], l2[1:])
            else:
                return a < b
        elif (not estEntier(a)) and (not estEntier(b)):
            try:
                return compare(a,b)
            except:
                return compare(l1[1:], l2[1:])
        elif estEntier(a):
            try:
                return compare([a], b)
            except:
                return compare(l1[1:], l2[1:])
        else:
            try:
                return compare(a, [b])
            except:
                return compare(l1[1:], l2[1:])

def comparebis(l1,l2):
    try:
        if compare(l1,l2):
            return -1
        else:
            return 1
    except:
        return 0

exo = 2
if exo == 1:
    s = 0
    for i in range(0,len(L),2):
        if compare(L[i], L[i+1]):
            s += 1+ i//2
    print(s)
else:
    L.append([[2]])
    L.append([[6]])
    from functools import cmp_to_key
    L = sorted(L, key=cmp_to_key(comparebis))
    p, i = 0, 0
    while L[i] != [[6]]:
        if L[i] == [[2]]:
            p = i+1
        i += 1
    print(p*(i+1))
