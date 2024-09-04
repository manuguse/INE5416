class No:
    def __init__(self, valor):
        self.valor = valor
        self.esquerda = None
        self.direita = None

def pre_order(raiz, resultado):
    if raiz is not None:
        resultado.append(str(raiz.valor))
        pre_order(raiz.esquerda, resultado)
        pre_order(raiz.direita, resultado)
        
def in_order(raiz, resultado):
    if raiz is not None:
        in_order(raiz.esquerda, resultado)
        resultado.append(str(raiz.valor))
        in_order(raiz.direita, resultado)
    
def post_order(raiz, resultado):
    if raiz is not None:
        post_order(raiz.esquerda, resultado)
        post_order(raiz.direita, resultado)
        resultado.append(str(raiz.valor))
        
def preenche_arvore(raiz, elemento):
    if raiz is None:
        return No(elemento)
    if elemento < raiz.valor:
        raiz.esquerda = preenche_arvore(raiz.esquerda, elemento)
    else:
        raiz.direita = preenche_arvore(raiz.direita, elemento)
    return raiz

c = int(input())
for i in range(c):
    n = int(input())
    elementos = list(map(int, input().split()))
    
    raiz = None
    for elemento in elementos:
        raiz = preenche_arvore(raiz, elemento)   
    
    print(f"Case {i+1}:")
    
    resultado = []
    pre_order(raiz, resultado)
    print("Pre.: " + " ".join(resultado))
    
    resultado = []
    in_order(raiz, resultado)
    print("In..: " + " ".join(resultado))
    
    resultado = []
    post_order(raiz, resultado)
    print("Post: " + " ".join(resultado))
    
    print("")
