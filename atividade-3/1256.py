n = int(input())
for i in range(n):
    enderecos, num_chaves = map(int, input().split())
    chaves = list(map(int, input().split()))
    mods = [[] for _ in range(enderecos)]
    
    for chave in chaves:
        mods[chave % enderecos].append(chave)
        
    for j, mod in enumerate(mods):
        print(j, end="")
        for m in mod:
            print(f" -> {m}", end="")
        print(" -> \\")
        
    if i < n - 1:
        print()