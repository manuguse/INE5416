while (True):
    try:
        expressao = input()
    except EOFError:
        break
    pilha = []
    for i in expressao:
        if i == '(':
            pilha.append(i)
        elif i == ')':
            if not pilha:
                pilha.append(i)
                break
            pilha.pop()
    print('correct' if not pilha else 'incorrect')