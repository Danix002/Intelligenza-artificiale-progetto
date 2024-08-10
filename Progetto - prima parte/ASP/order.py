import re

giornate = "partita(5,juventus,inter) partita(10,juventus,inter) partita(1,juventus,lazio) partita(9,juventus,lazio) partita(3,juventus,torino) partita(6,juventus,torino) partita(29,juventus,torino) partita(16,juventus,napoli) partita(13,juventus,fiorentina) partita(19,juventus,cagliari) partita(20,juventus,verona) partita(23,juventus,verona) partita(8,milan,juventus) partita(30,milan,juventus) partita(3,milan,roma) partita(11,milan,lazio) partita(27,milan,torino) partita(13,milan,bologna) partita(19,milan,bologna) partita(24,milan,bologna) partita(23,milan,udinese) partita(26,milan,udinese) partita(21,milan,genoa) partita(4,milan,atalanta) partita(6,milan,cagliari) partita(18,milan,cagliari) partita(25,milan,cagliari) partita(5,milan,verona) partita(14,milan,verona) partita(17,inter,juventus) partita(22,inter,juventus) partita(25,inter,juventus) partita(9,inter,milan) partita(12,inter,milan) partita(6,inter,roma) partita(21,inter,roma) partita(14,inter,lazio) partita(11,inter,torino) partita(15,inter,torino) partita(3,inter,bologna) partita(13,inter,udinese) partita(19,inter,udinese) partita(24,inter,napoli) partita(8,inter,genoa) partita(18,inter,fiorentina) partita(4,inter,cagliari) partita(2,inter,empoli) partita(23,inter,empoli) partita(30,inter,empoli) partita(28,inter,parma) partita(2,roma,juventus) partita(24,roma,juventus) partita(26,roma,juventus) partita(29,roma,milan) partita(13,roma,lazio) partita(12,roma,torino) partita(19,roma,torino) partita(22,roma,udinese) partita(25,roma,udinese) partita(27,roma,udinese) partita(4,roma,napoli) partita(9,roma,empoli) partita(7,roma,parma) partita(30,roma,parma) partita(26,lazio,inter) partita(18,lazio,roma) partita(6,lazio,bologna) partita(8,lazio,udinese) partita(10,lazio,napoli) partita(21,lazio,napoli) partita(28,lazio,napoli) partita(23,lazio,genoa) partita(7,lazio,fiorentina) partita(24,lazio,fiorentina) partita(27,lazio,fiorentina) partita(12,lazio,cagliari) partita(29,lazio,cagliari) partita(4,lazio,verona) partita(17,lazio,verona) partita(2,lazio,parma) partita(9,torino,bologna) partita(7,torino,udinese) partita(24,torino,udinese) partita(8,torino,napoli) partita(5,torino,genoa) partita(16,torino,genoa) partita(25,torino,genoa) partita(28,torino,genoa) partita(14,torino,fiorentina) partita(22,torino,fiorentina) partita(2,torino,atalanta) partita(30,torino,atalanta) partita(23,torino,cagliari) partita(26,torino,cagliari) partita(1,torino,empoli) partita(10,torino,empoli) partita(21,torino,empoli) partita(17,torino,parma) partita(20,bologna,inter) partita(14,bologna,roma) partita(15,bologna,roma) partita(30,bologna,lazio) partita(2,bologna,udinese) partita(22,bologna,napoli) partita(27,bologna,napoli) partita(18,bologna,genoa) partita(4,bologna,fiorentina) partita(23,bologna,fiorentina) partita(10,bologna,atalanta) partita(26,bologna,atalanta) partita(29,bologna,atalanta) partita(11,bologna,cagliari) partita(16,bologna,verona) partita(8,bologna,empoli) partita(17,bologna,empoli) partita(1,bologna,parma) partita(5,bologna,parma) partita(12,bologna,parma) partita(25,bologna,parma) partita(4,udinese,juventus) partita(12,udinese,juventus) partita(18,udinese,juventus) partita(1,udinese,milan) partita(15,udinese,lazio) partita(16,udinese,lazio) partita(20,udinese,lazio) partita(3,udinese,napoli) partita(6,udinese,napoli) partita(14,udinese,napoli) partita(9,udinese,atalanta) partita(11,udinese,atalanta) partita(29,udinese,empoli) partita(23,napoli,roma) partita(18,napoli,torino) partita(20,napoli,torino) partita(1,napoli,genoa) partita(2,napoli,genoa) partita(7,napoli,genoa) partita(9,napoli,genoa) partita(17,napoli,genoa) partita(12,napoli,fiorentina) partita(25,napoli,fiorentina) partita(11,napoli,verona) partita(29,napoli,verona) partita(30,napoli,verona) partita(26,napoli,empoli) partita(15,napoli,parma) partita(19,napoli,parma) partita(15,genoa,juventus) partita(27,genoa,inter) partita(29,genoa,inter) partita(11,genoa,roma) partita(3,genoa,lazio) partita(30,genoa,cagliari) partita(4,genoa,empoli) partita(6,genoa,empoli) partita(20,genoa,empoli) partita(22,genoa,empoli) partita(10,genoa,parma) partita(21,fiorentina,juventus) partita(28,fiorentina,juventus) partita(2,fiorentina,milan) partita(10,fiorentina,milan) partita(15,fiorentina,milan) partita(1,fiorentina,inter) partita(8,fiorentina,roma) partita(17,fiorentina,roma) partita(20,fiorentina,roma) partita(5,fiorentina,udinese) partita(30,fiorentina,udinese) partita(19,fiorentina,genoa) partita(3,fiorentina,verona) partita(29,fiorentina,parma) partita(17,atalanta,milan) partita(20,atalanta,milan) partita(7,atalanta,inter) partita(5,atalanta,roma) partita(16,atalanta,roma) partita(25,atalanta,lazio) partita(28,atalanta,udinese) partita(12,atalanta,genoa) partita(24,atalanta,genoa) partita(1,atalanta,cagliari) partita(8,atalanta,cagliari) partita(22,atalanta,cagliari) partita(6,atalanta,verona) partita(15,atalanta,verona) partita(18,atalanta,verona) partita(19,atalanta,verona) partita(21,atalanta,verona) partita(3,atalanta,empoli) partita(13,atalanta,empoli) partita(7,cagliari,juventus) partita(16,cagliari,inter) partita(10,cagliari,roma) partita(21,cagliari,bologna) partita(28,cagliari,bologna) partita(17,cagliari,udinese) partita(5,cagliari,napoli) partita(13,cagliari,napoli) partita(14,cagliari,genoa) partita(9,cagliari,fiorentina) partita(2,cagliari,verona) partita(27,cagliari,verona) partita(15,cagliari,empoli) partita(24,cagliari,empoli) partita(3,cagliari,parma) partita(20,cagliari,parma) partita(22,verona,milan) partita(28,verona,milan) partita(1,verona,roma) partita(7,verona,bologna) partita(10,verona,udinese) partita(13,verona,genoa) partita(26,verona,fiorentina) partita(25,verona,empoli) partita(8,verona,parma) partita(9,verona,parma) partita(11,empoli,juventus) partita(14,empoli,juventus) partita(7,empoli,milan) partita(28,empoli,roma) partita(5,empoli,lazio) partita(19,empoli,lazio) partita(16,empoli,fiorentina) partita(27,empoli,atalanta) partita(12,empoli,verona) partita(18,empoli,parma) partita(27,parma,juventus) partita(16,parma,milan) partita(22,parma,lazio) partita(4,parma,torino) partita(13,parma,torino) partita(21,parma,udinese) partita(26,parma,genoa) partita(6,parma,fiorentina) partita(11,parma,fiorentina) partita(14,parma,atalanta) partita(23,parma,atalanta) partita(24,parma,verona)"

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