import re

giornate = "partita(20,juventus,milan) partita(1,juventus,inter) partita(2,juventus,roma) partita(4,juventus,lazio) partita(5,juventus,torino) partita(7,juventus,bologna) partita(8,juventus,udinese) partita(10,juventus,napoli) partita(11,juventus,genoa) partita(13,juventus,fiorentina) partita(14,juventus,atalanta) partita(16,juventus,cagliari) partita(17,juventus,verona) partita(19,juventus,empoli) partita(22,juventus,parma) partita(3,milan,juventus) partita(2,milan,inter) partita(5,milan,roma) partita(6,milan,lazio) partita(8,milan,torino) partita(9,milan,bologna) partita(11,milan,udinese) partita(12,milan,napoli) partita(14,milan,genoa) partita(15,milan,fiorentina) partita(17,milan,atalanta) partita(21,milan,cagliari) partita(18,milan,verona) partita(22,milan,empoli) partita(12,inter,juventus) partita(10,inter,milan) partita(20,inter,roma) partita(3,inter,lazio) partita(4,inter,torino) partita(6,inter,bologna) partita(7,inter,udinese) partita(9,inter,napoli) partita(13,inter,genoa) partita(16,inter,fiorentina) partita(15,inter,atalanta) partita(23,inter,cagliari) partita(21,inter,verona) partita(18,inter,empoli) partita(15,roma,juventus) partita(13,roma,milan) partita(8,roma,inter) partita(1,roma,lazio) partita(7,roma,torino) partita(3,roma,bologna) partita(4,roma,udinese) partita(16,roma,napoli) partita(10,roma,genoa) partita(21,roma,fiorentina) partita(11,roma,cagliari) partita(23,roma,verona) partita(18,roma,parma) partita(21,lazio,juventus) partita(16,lazio,milan) partita(11,lazio,inter) partita(12,lazio,roma) partita(20,lazio,torino) partita(2,lazio,bologna) partita(5,lazio,udinese) partita(7,lazio,napoli) partita(8,lazio,genoa) partita(18,lazio,atalanta) partita(14,lazio,verona) partita(23,lazio,empoli) partita(18,torino,juventus) partita(23,torino,milan) partita(19,torino,inter) partita(9,torino,lazio) partita(10,torino,bologna) partita(22,torino,udinese) partita(1,torino,napoli) partita(3,torino,genoa) partita(6,torino,fiorentina) partita(15,torino,verona) partita(12,torino,empoli) partita(23,bologna,juventus) partita(14,bologna,inter) partita(22,bologna,roma) partita(13,bologna,lazio) partita(1,bologna,udinese) partita(20,bologna,napoli) partita(17,bologna,genoa) partita(4,bologna,fiorentina) partita(11,bologna,atalanta) partita(18,bologna,cagliari) partita(5,bologna,verona) partita(8,bologna,parma) partita(17,udinese,inter) partita(14,udinese,roma) partita(15,udinese,lazio) partita(12,udinese,bologna) partita(18,udinese,napoli) partita(23,udinese,genoa) partita(20,udinese,fiorentina) partita(21,udinese,atalanta) partita(9,udinese,cagliari) partita(2,udinese,verona) partita(3,udinese,empoli) partita(6,udinese,parma) partita(22,napoli,inter) partita(6,napoli,roma) partita(17,napoli,lazio) partita(14,napoli,torino) partita(19,napoli,genoa) partita(23,napoli,fiorentina) partita(5,napoli,atalanta) partita(8,napoli,cagliari) partita(11,napoli,empoli) partita(3,napoli,parma) partita(9,genoa,roma) partita(21,genoa,torino) partita(4,genoa,napoli) partita(18,genoa,fiorentina) partita(1,genoa,atalanta) partita(20,genoa,cagliari) partita(7,genoa,verona) partita(15,genoa,empoli) partita(12,genoa,parma) partita(5,fiorentina,inter) partita(22,fiorentina,lazio) partita(11,fiorentina,torino) partita(19,fiorentina,bologna) partita(2,fiorentina,napoli) partita(8,fiorentina,atalanta) partita(10,fiorentina,verona) partita(14,fiorentina,empoli) partita(17,fiorentina,parma) partita(9,atalanta,juventus) partita(4,atalanta,milan) partita(19,atalanta,roma) partita(13,atalanta,torino) partita(16,atalanta,genoa) partita(12,atalanta,fiorentina) partita(2,atalanta,cagliari) partita(20,atalanta,verona) partita(6,atalanta,empoli) partita(23,atalanta,parma) partita(7,cagliari,milan) partita(19,cagliari,lazio) partita(17,cagliari,torino) partita(10,cagliari,udinese) partita(15,cagliari,napoli) partita(6,cagliari,genoa) partita(3,cagliari,fiorentina) partita(22,cagliari,atalanta) partita(12,cagliari,verona) partita(4,cagliari,empoli) partita(14,cagliari,parma) partita(6,verona,juventus) partita(16,verona,bologna) partita(19,verona,udinese) partita(13,verona,napoli) partita(22,verona,genoa) partita(9,verona,fiorentina) partita(3,verona,atalanta) partita(1,verona,cagliari) partita(8,verona,empoli) partita(11,verona,parma) partita(1,empoli,milan) partita(17,empoli,roma) partita(2,empoli,torino) partita(21,empoli,bologna) partita(16,empoli,udinese) partita(5,empoli,genoa) partita(7,empoli,fiorentina) partita(10,empoli,atalanta) partita(13,empoli,cagliari) partita(20,empoli,parma) partita(19,parma,milan) partita(10,parma,lazio) partita(16,parma,torino) partita(15,parma,bologna) partita(13,parma,udinese) partita(21,parma,napoli) partita(2,parma,genoa) partita(1,parma,fiorentina) partita(7,parma,atalanta) partita(5,parma,cagliari) partita(4,parma,verona) partita(9,parma,empoli)"

def estrai_squadre(partita):
    # Usa una regex per catturare i nomi delle due squadre
    match = re.match(r"partita\(\d+,(.+),(.+)\)", partita)
    if match:
        squadra1 = match.group(1)
        squadra2 = match.group(2)
        return f"{squadra1} - {squadra2}"
    return ""

def ordinaGiornate():
    giornateArray = giornate.split(" ")
    
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