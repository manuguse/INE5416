while True:
    n = int(input())
    if n == 0:
        break
    magnitudes = list(map(int, input().split()))
    picos = 0 # vai ser pico se for máximo ou mínimo local
    for i in range(n):
        if magnitudes[i] < magnitudes[(i - 1) % n] and magnitudes[i] < magnitudes[(i + 1) % n]:
            picos += 1
        elif magnitudes[i] > magnitudes[(i - 1) % n] and magnitudes[i] > magnitudes[(i + 1) % n]:
            picos += 1
    print(picos)