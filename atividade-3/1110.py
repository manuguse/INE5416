while True:
    n = int(input())
    if n == 0:
        break
    pilha = []
    cartas_descartadas = []
    for i in range(n):
        pilha.append(i+1)
    while len(pilha) > 1:
        cartas_descartadas.append(pilha.pop(0))
        pilha.append(pilha.pop(0))
    print("Discarded cards:", ", ".join(map(str, cartas_descartadas)))
    print(f"Remaining card: {pilha[0]}")
