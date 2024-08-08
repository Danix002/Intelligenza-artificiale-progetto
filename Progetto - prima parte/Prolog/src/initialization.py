import tkinter as tk

def create_interface():
    # Callback function to handle button clicks
    def on_button_click(choice):
        result.set(choice)
        root.destroy()

    # Create the main window
    root = tk.Tk()
    root.title("Scelta della strategia da utilizzare per risolvere il labirinto")

    # Create a StringVar to store the result
    result = tk.StringVar()

    # Create buttons with different options
    button1 = tk.Button(root, text="Base first strategy", command=lambda: on_button_click("Opzione 1"))
    button2 = tk.Button(root, text="Base second strategy", command=lambda: on_button_click("Opzione 2"))
    button3 = tk.Button(root, text="Final first strategy", command=lambda: on_button_click("Opzione 3"))
    button4 = tk.Button(root, text="Final second strategy", command=lambda: on_button_click("Opzione 4"))

    # Pack buttons into the window
    button1.pack(pady=10)
    button2.pack(pady=10)
    button3.pack(pady=10)
    button4.pack(pady=10)

    # Start the Tkinter main loop
    root.mainloop()

    # Return the selected result
    print(result.get())
    return result.get()