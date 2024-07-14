import tkinter as tk
import janus_swi as j

# Inizializza la sessione di Janus
j.consult("labirinto_first_strategy.pl")

w = "wall"
h = "hammer"
dw = "destriyable-wall"
g = "gem"
p = "portal"
mp = "monster-position"

# Configurazione del labirinto
labirinto = [
    [w, h, " ", " ", " ", " ", " ", g],
    [" ", " ", " ", " ", w, " ", " ", " "],
    [" ", p, " ", " ", " ", " ", " ", " "],
    [" ", " ", " ", " ", " ", " ", " ", " "],
    [" ", " ", " ", w, g, " ", " ", " "],
    [" ", " ", " ", " ", w, w, " ", w],
    [" ", dw, dw, dw, w, " ", " ", mp],
    [w, g, " ", " ", w, w, " ", " "]
]

# Funzione per disegnare il labirinto
def disegna_labirinto(canvas, labirinto):
    for riga in range(len(labirinto)):
        for colonna in range(len(labirinto[riga])):
            x0 = colonna * 40
            y0 = riga * 40
            x1 = x0 + 40
            y1 = y0 + 40

            if labirinto[riga][colonna] == w:
                canvas.create_rectangle(x0, y0, x1, y1, fill="black")

            if labirinto[riga][colonna] == dw:
                canvas.create_rectangle(x0, y0, x1, y1, fill="grey")

            if labirinto[riga][colonna] == g:
                canvas.create_rectangle(x0, y0, x1, y1, fill="purple") 
            
            if labirinto[riga][colonna] == h:
                canvas.create_rectangle(x0, y0, x1, y1, fill="green") 

            if labirinto[riga][colonna] == p:
                canvas.create_rectangle(x0, y0, x1, y1, fill="yellow") 
            
            if labirinto[riga][colonna] == mp:
                canvas.create_rectangle(x0, y0, x1, y1, fill="red") 

            if labirinto[riga][colonna] == " ":
                canvas.create_rectangle(x0, y0, x1, y1, fill="white")

# Funzione per aggiornare il labirinto con la soluzione trovata
def aggiorna_labirinto(labirinto, soluzione):
    for state in soluzione:
        pos = state[0]
        x, y = pos[0], pos[1]
        labirinto[y-1][x-1] = "O"

# Funzione per risolvere il labirinto con Prolog
def risolvi_labirinto():
    result = j.query('ricerca(Cammino)')
    print(result.state)
    """    
    if result:
        cammino = result.state
        aggiorna_labirinto(labirinto, cammino)
        canvas.delete("all")
        disegna_labirinto(canvas, labirinto)
        print("Soluzione trovata:", cammino)
    else:
        print("Nessuna soluzione trovata")
    """

# Configurazione della finestra tkinter
root = tk.Tk()
root.title("Labirinto")
canvas = tk.Canvas(root, width=320, height=320)
canvas.pack()

# Disegna il labirinto
disegna_labirinto(canvas, labirinto)

# Bottone per risolvere il labirinto
button = tk.Button(root, text="Risolvi Labirinto", command=risolvi_labirinto)
button.pack()

# Avvia la finestra
root.mainloop()


