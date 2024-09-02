n = int(input())
casas = []
for i in range(n):
    casas.append(int(input()))
soma = int(input())

i = 0
j = n - 1

while i < j:
    if casas[i] + casas[j] == soma:
        print(f'{casas[i]} {casas[j]}')
        break
    elif casas[i] + casas[j] < soma:
        i += 1
    else:
        j -= 1