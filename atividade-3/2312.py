n = int(input())
paises = []
for i in range(n):
    pais, ouro, prata, bronze = input().split()
    paises.append((pais, int(ouro), int(prata), int(bronze)))
    
paises.sort(key=lambda x: (-x[1], -x[2], -x[3], x[0]))

for pais in paises:
    print(pais[0], pais[1], pais[2], pais[3])
