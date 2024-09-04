n = int(input())
for i in range(n):
    mina = input()
    pilha = []
    diamantes = 0
    for c in mina:
        if c == '<':
            pilha.append(c)
        elif c == '>' and pilha:
            pilha.pop()
            diamantes += 1   
    print(diamantes)