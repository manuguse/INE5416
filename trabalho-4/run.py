import subprocess

# Caminho para o arquivo Prolog
caminho_arquivo_prolog = '33.pl'

# Comando para executar o arquivo Prolog usando o SWI-Prolog
comando = f'swipl -s {caminho_arquivo_prolog} -g "solucao(ListaSolucao), halt."'

# Executa o comando e captura a saída
resultado = subprocess.run(comando, shell=True, text=True, capture_output=True)

# Imprime a saída do Prolog
print("Saída do Prolog:")
print(resultado.stdout)

# Imprime qualquer erro que tenha ocorrido
if resultado.stderr:
    print("Erros:")
    print(resultado.stderr)
