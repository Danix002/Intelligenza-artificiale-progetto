import re

giornate = (
    "partita(11,juventus,milan) partita(18,juventus,inter) partita(13,juventus,roma) "
    "partita(22,juventus,lazio) partita(20,juventus,torino) partita(15,juventus,bologna) "
    "partita(1,juventus,udinese) partita(2,juventus,napoli) partita(23,juventus,genoa) "
    "partita(10,juventus,fiorentina) partita(4,juventus,atalanta) partita(5,juventus,cagliari) "
    "partita(7,juventus,verona) partita(8,juventus,empoli) partita(16,juventus,parma) "
    "partita(3,milan,juventus) partita(13,milan,inter) partita(1,milan,roma) "
    "partita(4,milan,lazio) partita(23,milan,torino) partita(6,milan,bologna) "
    "partita(7,milan,udinese) partita(16,milan,napoli) partita(18,milan,genoa) "
    "partita(9,milan,fiorentina) partita(20,milan,atalanta) partita(15,milan,verona) "
    "partita(10,milan,empoli) partita(2,inter,milan) partita(19,inter,roma) "
    "partita(17,inter,lazio) partita(5,inter,torino) partita(8,inter,bologna) "
    "partita(14,inter,genoa) partita(21,inter,atalanta) partita(11,inter,cagliari) "
    "partita(22,inter,verona) partita(12,inter,parma) partita(6,roma,juventus) "
    "partita(21,roma,milan) partita(3,roma,inter) partita(7,roma,lazio) "
    "partita(17,roma,torino) partita(11,roma,bologna) partita(18,roma,udinese) "
    "partita(4,roma,napoli) partita(10,roma,atalanta) partita(14,roma,cagliari) "
    "partita(15,roma,empoli) partita(9,lazio,juventus) partita(12,lazio,milan) "
    "partita(1,lazio,inter) partita(16,lazio,roma) partita(2,lazio,torino) "
    "partita(5,lazio,bologna) partita(13,lazio,udinese) partita(8,lazio,napoli) "
    "partita(20,lazio,cagliari) partita(19,lazio,verona) partita(23,lazio,parma) "
    "partita(8,torino,milan) partita(16,torino,inter) partita(13,torino,bologna) "
    "partita(21,torino,udinese) partita(19,torino,napoli) partita(1,torino,genoa) "
    "partita(4,torino,fiorentina) partita(14,torino,atalanta) partita(10,torino,cagliari) "
    "partita(11,torino,verona) partita(22,torino,empoli) partita(7,torino,parma) "
    "partita(14,bologna,milan) partita(23,bologna,inter) partita(10,bologna,lazio) "
    "partita(3,bologna,torino) partita(16,bologna,udinese) partita(20,bologna,genoa) "
    "partita(19,bologna,fiorentina) partita(7,bologna,atalanta) partita(12,bologna,verona) "
    "partita(4,bologna,empoli) partita(17,bologna,parma) partita(17,udinese,milan) "
    "partita(10,udinese,inter) partita(2,udinese,roma) partita(6,udinese,lazio) "
    "partita(22,udinese,napoli) partita(11,udinese,genoa) partita(14,udinese,fiorentina) "
    "partita(8,udinese,atalanta) partita(3,udinese,cagliari) partita(23,udinese,verona) "
    "partita(20,udinese,empoli) partita(19,udinese,parma) partita(17,napoli,juventus) "
    "partita(20,napoli,inter) partita(23,napoli,roma) partita(14,napoli,lazio) "
    "partita(9,napoli,udinese) partita(6,napoli,genoa) partita(13,napoli,fiorentina) "
    "partita(11,napoli,atalanta) partita(21,napoli,cagliari) partita(3,napoli,empoli) "
    "partita(5,napoli,parma) partita(22,genoa,roma) partita(21,genoa,lazio) "
    "partita(12,genoa,torino) partita(9,genoa,bologna) partita(4,genoa,udinese) "
    "partita(15,genoa,napoli) partita(16,genoa,fiorentina) partita(2,genoa,atalanta) "
    "partita(13,genoa,cagliari) partita(5,genoa,verona) partita(19,genoa,empoli) "
    "partita(8,genoa,parma) partita(15,fiorentina,inter) partita(12,fiorentina,roma) "
    "partita(11,fiorentina,lazio) partita(18,fiorentina,torino) partita(1,fiorentina,bologna) "
    "partita(5,fiorentina,udinese) partita(7,fiorentina,napoli) partita(3,fiorentina,genoa) "
    "partita(8,fiorentina,cagliari) partita(17,fiorentina,empoli) partita(21,fiorentina,parma) "
    "partita(19,atalanta,juventus) partita(5,atalanta,milan) partita(6,atalanta,inter) "
    "partita(9,atalanta,roma) partita(15,atalanta,lazio) partita(22,atalanta,bologna) "
    "partita(12,atalanta,udinese) partita(23,atalanta,fiorentina) partita(17,atalanta,cagliari) "
    "partita(3,atalanta,verona) partita(1,atalanta,parma) partita(12,cagliari,juventus) "
    "partita(19,cagliari,milan) partita(9,cagliari,inter) partita(18,cagliari,bologna) "
    "partita(15,cagliari,udinese) partita(1,cagliari,napoli) partita(7,cagliari,genoa) "
    "partita(22,cagliari,fiorentina) partita(16,cagliari,verona) partita(6,cagliari,empoli) "
    "partita(4,cagliari,parma) partita(21,verona,juventus) partita(4,verona,inter) "
    "partita(8,verona,roma) partita(6,verona,torino) partita(10,verona,napoli) "
    "partita(17,verona,genoa) partita(20,verona,fiorentina) partita(18,verona,atalanta) "
    "partita(2,verona,cagliari) partita(13,verona,empoli) partita(14,verona,parma) "
    "partita(14,empoli,juventus) partita(7,empoli,inter) partita(5,empoli,roma) "
    "partita(18,empoli,lazio) partita(9,empoli,torino) partita(21,empoli,bologna) "
    "partita(12,empoli,napoli) partita(2,empoli,fiorentina) partita(16,empoli,atalanta) "
    "partita(23,empoli,cagliari) partita(1,empoli,verona) partita(11,empoli,parma) "
    "partita(22,parma,milan) partita(20,parma,roma) partita(3,parma,lazio) "
    "partita(15,parma,torino) partita(2,parma,bologna) partita(18,parma,napoli) "
    "partita(10,parma,genoa) partita(6,parma,fiorentina) partita(13,parma,atalanta) "
    "partita(9,parma,verona)"
)

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