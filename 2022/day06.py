f = open("input", "r")

n = 14
Lines = f.readlines()

for line in Lines:
    i = 0
    test = True
    while test:
        test = False
        for j in range(n):
            if line[i+j+1:i+n].__contains__(line[i+j]):
                test = True
                break
        i+=1
    print(i-1 + n)
