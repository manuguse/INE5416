import os
import re
import requests

boards_dir = './boards'
blocks_dir = './blocks'
expected_dir = './expected'

os.makedirs(boards_dir, exist_ok=True)
os.makedirs(blocks_dir, exist_ok=True)
os.makedirs(expected_dir, exist_ok=True)

base_url = 'https://www.janko.at/Raetsel/Kojun/{}.a.htm'

def download_and_save_page(num):
    url = base_url.format(num.zfill(3))
    response = requests.get(url)
    if response.status_code != 200:
        print(f"Erro ao acessar {url}: {response.status_code}")
        return
    
    content = response.text

    try:
        with open(f'{boards_dir}/{num}.txt', 'w', encoding='utf-8') as f:
            f.write(extract_section(content, '[problem]'))
        
        with open(f'{blocks_dir}/{num}.txt', 'w', encoding='utf-8') as f:
            f.write(extract_section(content, '[areas]'))
        
        with open(f'{expected_dir}/{num}.txt', 'w', encoding='utf-8') as f:
            f.write(extract_section(content, '[solution]'))
    except Exception as e:
        print(f"Erro ao salvar arquivos para {num}: {e}")

def extract_section(content, section_name):
    pattern = re.compile(re.escape(section_name) + r'(.*?)\n\[', re.DOTALL)
    match = pattern.search(content)
    if match:
        return match.group(1).strip()
    else:
        return f"Seção {section_name} não encontrada."

for i in range(1, 111):
    download_and_save_page(str(i))

print("Download concluído.")
