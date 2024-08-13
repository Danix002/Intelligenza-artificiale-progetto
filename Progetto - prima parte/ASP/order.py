import re
import json

def extract_values_from_json(file_path):
    """
    Extract the 'Value' entries from a given JSON file.

    Parameters:
    - file_path (str): Path to the JSON file.

    Returns:
    - List of values if successful, or an empty list if an error occurs.
    """
    try:
        # Read and load the JSON data
        with open(file_path, 'r') as file:
            data = json.load(file)

        # Extract the "Value" from the JSON structure
        value_list = data["Call"][0]["Witnesses"][0]["Value"]
        return value_list

    except (KeyError, json.JSONDecodeError, FileNotFoundError) as e:
        print(f"Error: {e}")
        return []
    

def estrai_squadre(partita):
    # Usa una regex per catturare i nomi delle due squadre
    match = re.match(r"partita\(\d+,(.+),(.+)\)", partita)
    if match:
        squadra1 = match.group(1)
        squadra2 = match.group(2)
        return f"{squadra1} - {squadra2}"
    return ""

def ordinaGiornate():
    giornateArray = extract_values_from_json("calendario.json")
    sorted = ordina_partite(giornateArray)
    count = 0
    for partita in sorted:
        if count % 8 == 0:
            print("Gionata ", int((count / 8)+1))
        print("  ",estrai_squadre(partita))
        count += 1
    
def ordina_partite(partite):
    # Funzione per estrarre il numero della giornata dalla stringa
    def estrai_giornata(partita):
        match = re.match(r"partita\((\d+),", partita)
        if match:
            return int(match.group(1))
        return 0
    # Ordina l'array utilizzando il numero della giornata
    partite_ordinate = sorted(partite, key=estrai_giornata)
    
    return partite_ordinate 
     
ordinaGiornate()