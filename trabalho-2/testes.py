import os
import subprocess

os.system("scalac -d out/ src/*.scala")

if not os.access("./run.sh", os.X_OK):
    os.chmod("./run.sh", 0o755)

def normalize_output(output):
    return "\n".join(line.strip() for line in output.strip().splitlines())

for i in range(1, 111):
    result = subprocess.run(["./run.sh", str(i)], stdout=subprocess.PIPE)
    output = result.stdout.decode('utf-8')
    
    if "Solução:" in output:
        output = normalize_output(output.split("Solução:")[1].split("P")[0])
    else:
        output = normalize_output(output)
    
    expected_output = normalize_output(open(f"expected/{i}.txt", "r").read())
    assert output == expected_output, f"Test {i} failed: output({output}) != expected({expected_output})"
    # print(f"resultado:\n{output}\nesperado:\n{expected_output}")
    print(f"Teste {i} passou")
