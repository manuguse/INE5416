def insere_arvore(arvore, elemento):
    if not arvore:
        return [elemento, [], []]
    if elemento < arvore[0]:
        arvore[1] = insere_arvore(arvore[1], elemento)
    else:
        arvore[2] = insere_arvore(arvore[2], elemento)
    return arvore

c = int(input())
for i in range(c):
    n = int(input())
    ordem_arvore = []
    elementos = list(map(int, input().split()))
    
    for elemento in elementos:
        ordem_arvore = insere_arvore(ordem_arvore, elemento)
        
    fila = [ordem_arvore]
    nivel = []
    while fila:
        node = fila.pop(0)
        nivel.append(node[0])
        if node[1]:
            fila.append(node[1])
        if node[2]:
            fila.append(node[2])
            
        
    print(f"Case {i+1}:")
    for j in range(len(nivel)):
        print(nivel[j], end="")
        if j < len(nivel) - 1:
            print(" ", end="")
    print("\n")