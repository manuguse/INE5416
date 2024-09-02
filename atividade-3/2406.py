t = int(input())
for i in range(t):
    expressao = input()
    pilha = []
    relacao = {
        ']': '[',
        '}': '{',
        ')': '('
    }
    for i in expressao:
        if i in relacao.values():
            pilha.append(i)
        elif i in relacao.keys():
            if not pilha or pilha.pop() != relacao[i]:
                pilha.append(i)
                break
        
    print('S' if not pilha else 'N')