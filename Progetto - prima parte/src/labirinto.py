import tkinter as tk
from tkinter import font
from PIL import Image, ImageTk
from pyswip import Prolog

# Inizializza la sessione di Prolog
prolog = Prolog()
prolog.consult("src/labirinto_first_strategy.pl")

w = "wall"
h = "hammer"
hg = "hammer-gem"
dw = "destroyable-wall"
g = "gem"
p = "portal"
mp = "monster-position"

# Posizione iniziale del mostro
posizione_mostro = [6, 7]

# Configurazione della finestra tkinter
root = tk.Tk()
root.title("Labirinto")

# Funzione per caricare immagini
def carica_immagine(file):
    img = Image.open(file)
    img = img.resize((79, 79), Image.ADAPTIVE)  # Ridimensiona a 79x79 per adattarsi meglio a 80x80 pixel
    return ImageTk.PhotoImage(img)

immagini = {
    w: carica_immagine("src/img/wall.png"),
    h: carica_immagine("src/img/hammer.png"),
    dw: carica_immagine("src/img/destroyable-wall.png"),
    g: carica_immagine("src/img/gem.png"),
    p: carica_immagine("src/img/portal.png"),
    mp: carica_immagine("src/img/monster-position.png"),
    " ": carica_immagine("src/img/empty-alternative.jpg"),
    hg: carica_immagine("src/img/hammer-gem.png")  
}

# Configurazione del labirinto
labirinto = [
    [w, h, " ", " ", " ", " ", " ", g],
    [" ", " ", " ", " ", w, " ", " ", " "],
    [" ", " ", " ", " ", " ", " ", " ", " "],
    [" ", " ", " ", " ", " ", " ", " ", " "],
    [" ", " ", " ", w, g, " ", " ", " "],
    [" ", " ", " ", " ", w, w, " ", w],
    [" ", dw, dw, dw, w, " ", " ", mp],
    [w, g, " ", p, w, w, " ", " "]
]

canvas_items = []

# Funzione per disegnare il labirinto
def disegna_labirinto(canvas, labirinto):
    canvas_items.clear()
    for riga in range(len(labirinto)):
        canvas_items_row = []
        for colonna in range(len(labirinto[riga])):
            x0 = colonna * 80
            y0 = riga * 80
            item = canvas.create_image(x0, y0, anchor=tk.NW, image=immagini[" "])
            item = canvas.create_image(x0, y0, anchor=tk.NW, image=immagini[labirinto[riga][colonna]])
            canvas_items_row.append(item)
        canvas_items.append(canvas_items_row)

def get_first_solution(prolog, query):
    first_result = []
    # Esegui la query
    result_generator = prolog.query(query)
    # Estrai solo il primo risultato
    first_result = next(result_generator, None)
    return first_result

# Funzione per aggiornare il labirinto con la direzione del movimento
def aggiorna_labirinto(labirinto, direction, final_visited, gem_states):
    global posizione_mostro
    x, y = posizione_mostro[0], posizione_mostro[1]
    print(final_visited)

    def muovi_mostro_e_gemme(i):
        if i >= len(final_visited):
            button.config(state=tk.NORMAL)  
            return
        
        dir = direction[i-1]
        from_visited_x, from_visited_y = generate_coordinate_from_pos(final_visited[i-1])
        to_visited_x, to_visited_y = generate_coordinate_from_pos(final_visited[i])
        
        # Ottieni le coordinate delle celle da visitare
        x1, y1 = from_visited_x, from_visited_y
        x2, y2 = to_visited_x, to_visited_y

        coordinates = bresenham(x1, y1, x2, y2)
        
        # Definisci i movimenti per ciascuna direzione
        if dir == 'nord':
            if(len(coordinates) > 0):
                for x, y in coordinates:
                    canvas.itemconfig(canvas_items[x][y], image=immagini[" "])
            canvas.itemconfig(canvas_items[x2][y2], image=immagini[mp])
        elif dir == 'sud':
            if(len(coordinates) > 0):
                for x, y in coordinates:
                    canvas.itemconfig(canvas_items[x][y], image=immagini[" "])
            canvas.itemconfig(canvas_items[x2][y2], image=immagini[mp])
        elif dir == 'ovest':
            if(len(coordinates) > 0):
                for x, y in coordinates:
                    canvas.itemconfig(canvas_items[x][y], image=immagini[" "])
            canvas.itemconfig(canvas_items[x2][y2], image=immagini[mp])
        elif dir == 'est':
            if(len(coordinates) > 0):
                for x, y in coordinates:
                    canvas.itemconfig(canvas_items[x][y], image=immagini[" "])
            canvas.itemconfig(canvas_items[x2][y2], image=immagini[mp])

        for gem in gem_states[i-1]:
            old_x, old_y = generate_coordinate_from_pos(gem)
            obstacle_detector_flag = len(obstacle_detector([(old_x, old_y)], targets={h})) > 0 
            if(obstacle_detector_flag):
                canvas.itemconfig(canvas_items[old_x][old_y], image=immagini[h])
            else:
                canvas.itemconfig(canvas_items[old_x][old_y], image=immagini[" "])

        for gem in gem_states[i]:
            new_x, new_y = generate_coordinate_from_pos(gem)
            obstacle_detector_flag = len(obstacle_detector([(new_x, new_y)], targets={h})) > 0 
            if(obstacle_detector_flag):
                canvas.itemconfig(canvas_items[new_x][new_y], image=immagini[hg])
            else:
                canvas.itemconfig(canvas_items[new_x][new_y], image=immagini[g])
            
        root.after(1500, muovi_mostro_e_gemme, i+1)
        
        # Aggiorna la posizione del mostro
        posizione_mostro[0], posizione_mostro[1] = x2, y2
    
    muovi_mostro_e_gemme(1)

def bresenham(x1, y1, x2, y2):
    # Lista delle coordinate intermedie
    coordinates = []
    # Calcolo delle differenze e dei passi
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)
    sx = 1 if x1 < x2 else -1
    sy = 1 if y1 < y2 else -1
    err = dx - dy
    while True:
        # Aggiungi la coordinata corrente alla lista
        coordinates.append((x1, y1))
        # Se siamo arrivati al punto finale, termina il ciclo
        if x1 == x2 and y1 == y2:
            break
        e2 = 2 * err
        if e2 > -dy:
            err -= dy
            x1 += sx
        if e2 < dx:
            err += dx
            y1 += sy
    return coordinates

def obstacle_detector(coordinates, targets):
    obstacle_positions = []
    for x, y in coordinates:
        if labirinto[x][y] in targets:
            obstacle_positions.append((x, y))
    return obstacle_positions

def extract_monster_position(final_visited):
    monster_positions = []
    for pos in final_visited:
        mpos = pos[0]
        monster_positions.append(mpos)
    return monster_positions

def generate_coordinate_from_pos(position):
    # Form the string 'pos(monster_position, x, y)' in position extract the cordinate
    x = position.split(',')[1]
    y = position.split(',')[2][:-1]
    return int(x), int(y)
    
# Funzione per risolvere il labirinto con Prolog
def risolvi_labirinto():
    # Button cant't clicked
    button.config(state=tk.DISABLED)

    disegna_labirinto(canvas, labirinto)

    first_result = get_first_solution(prolog, "ricerca_iterative_deepening(Cammino, GemStates, FinalVisited)")
    final_visited = extract_monster_position(first_result['FinalVisited'])

    if first_result:
        aggiorna_labirinto(labirinto, first_result['Cammino'], final_visited[::-1], first_result['GemStates'])
        print("Soluzione trovata:", first_result['Cammino'])
    else:
        print("Nessuna soluzione trovata")
        # Button can be clicked
        button.config(state=tk.NORMAL)

# Configurazione del canvas
canvas = tk.Canvas(root, width=640, height=640)  # Dimensione del canvas aumentata
canvas.pack()

# Disegna il labirinto
disegna_labirinto(canvas, labirinto)

button_font = font.Font(family='Minecraft', size=14, weight='bold')
button = tk.Button(root, text="Risolvi Labirinto", command=risolvi_labirinto,
                   font=button_font, bg='green', fg='white', padx=10, pady=5,
                   borderwidth=5, relief=tk.RAISED)
button.pack(pady=20)

# Avvia la finestra
root.mainloop()
