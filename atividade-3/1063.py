while True:
    n = int(input())
    if n == 0:
        break
    
    vagoes_a = list(input().split())
    sequencia_b = list(input().split())
    
    pilha = []
    
    i = 0
    j = 0
    
    while True:
        if pilha and pilha[-1] == sequencia_b[j]:
            pilha.pop()
            print("R", end="")
            j += 1
        elif i < n:
            pilha.append(vagoes_a[i])
            print("I", end="")
            i += 1
        else:
            break
    if pilha:
        print(" Impossible", end="")
    print()
