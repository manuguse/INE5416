import os
import subprocess
import time

# Compila os arquivos Scala para o diretório `out`
os.system("scalac -d out/ src/*.scala")

# Garante que o script `run.sh` seja executável
if not os.access("./run.sh", os.X_OK):
    os.chmod("./run.sh", 0o755)

# Função para normalizar o output removendo espaços e formatando linhas
def normalize_output(output):
    return "\n".join(line.strip() for line in output.strip().splitlines())

# Listas para armazenar resultados dos testes
success_tests = []
failed_tests = []

# Registra o tempo total de execução
start_time = time.time()

# Loop para executar e verificar os testes de 1 a 110
for i in range(1, 110):
    test_start_time = time.time()  # Tempo de início do teste
    
    # Executa o teste
    try:
        result = subprocess.run(["./run.sh", str(i)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
        output = result.stdout.decode('utf-8')
    except subprocess.CalledProcessError as e:
        print(f"Erro ao executar o teste {i}: {e}")
        failed_tests.append((i, 0, "Erro de execução", "N/A"))
        continue

    # Processa o output do programa
    if "Solução:" in output:
        output = normalize_output(output.split("Solução:")[1].split("P")[0])
    else:
        output = normalize_output(output)

    # Lê o resultado esperado
    try:
        expected_output = normalize_output(open(f"expected/{i}.txt", "r").read())
    except FileNotFoundError:
        print(f"Arquivo esperado para o teste {i} não encontrado.")
        failed_tests.append((i, 0, output, "Arquivo esperado ausente"))
        continue

    test_end_time = time.time()  # Tempo de término do teste
    execution_time = test_end_time - test_start_time

    # Compara os resultados
    if output == expected_output:
        success_tests.append((i, execution_time))
        print(f"Teste {i} passou em {execution_time:.2f} segundos")
    else:
        failed_tests.append((i, execution_time, output, expected_output))
        print(f"Teste {i} falhou em {execution_time:.2f} segundos: output({output}) != esperado({expected_output})")

# Calcula o tempo total de execução
end_time = time.time()
total_execution_time = end_time - start_time

# Gera um relatório dos testes
with open("test_report.txt", "w") as report_file:
    report_file.write(f"Tempo total de execução: {total_execution_time:.2f} segundos\n\n")
    report_file.write("Testes bem-sucedidos:\n")
    for test, exec_time in success_tests:
        report_file.write(f"Teste {test}: {exec_time:.2f} segundos\n")

    report_file.write("\nTestes falhados:\n")
    for test, exec_time, output, expected_output in failed_tests:
        report_file.write(f"Teste {test}: {exec_time:.2f} segundos\n")
        report_file.write(f"Output: {output}\n")
        report_file.write(f"Esperado: {expected_output}\n\n")

print(f"Relatório salvo em 'test_report.txt'.")
