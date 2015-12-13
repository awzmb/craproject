######################## LIBRARIES #############################
library(parallel)




######################## VARIABLEN #############################
#Wert fuer kNN (Anzahl an Neighbours)
#kValues = c(2, 3, 5, 10, 20, 50, 100, 1682)
kValues = c(
  1682 * 0.05,
  1682 * 0.10,
  1682 * 0.15,
  1682 * 0.20,
  1682 * 0.25,
  1682 * 0.30,
  1682 * 0.35,
  1682 * 0.40,
  1682 * 0.45,
  1682 * 0.50,
  1682 * 0.55,
  1682 * 0.60,
  1682 * 0.65,
  1682 * 0.70,
  1682 * 0.75,
  1682 * 0.80,
  1682 * 0.85,
  1682 * 0.90,
  1682 * 0.95
)
kValues <- round(kValues)




######################## DATENIMPORT #############################
#u.data (gesamte Datenmenge)
uData <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(uData) <- c("UserID", "ItemID", "Rating", "Timestamp")

# Xtab aus u.data erstellen um daraus activeUser zu ziehen
# (ACHTUNG: Anzahl Columns nicht identisch -> muss abgeschnitten werden)
#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
uDataXtab <- xtabs(Rating ~ UserID + ItemID, uData)
#Nullwerte durch NA ersetzen
uDataXtab[uDataXtab == 0] = NA
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(uDataXtab) = "matrix"

#Beschreibung der Items
itemDescription <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(itemDescription) <- c("ItemID", "Name")

#Vektoren mit den Namen fuer den spaeteren Zugriff erzeugen
userMatrixNames <- NULL
uDataActiveUserNames <- NULL
spearmanMatrixNames <- NULL


#Erstellen der benoetigten Matrizen
for(dataSetVar in 1:5)
{
  #Userbewertungen
  userMatrix <- as.matrix(read.table(file = paste0("ML100k/u", dataSetVar, ".user.items.test.csv"), sep = ";"))
  
  #Relevante UserIDs
  trainingUsers <- read.table(file = paste0("ML100k/u", dataSetVar, ".user.training.csv"), sep = ";")
  
  #Teilen von uData in 2 separate Matrizen (1x mit den relevanten UserIDs (Trainingsuser)
  #und 1x mit den Testusern)
  uDataActiveUser <- uDataXtab[(rownames(uDataXtab) %in% userMatrix[,1]),]
  uDataSpearman <- uDataXtab[(rownames(uDataXtab) %in% trainingUsers[,1]),]
  
  #Aehnlichkeitsmatrix berechnen (Spearman)
  spearmanMatrix <- cor(uDataSpearman, use = "pairwise.complete.obs", method="spearman")
  
  #Diagonale der Aehnlichkeitsmatrix durch NA ersetzen (aehnlichkeit der Diagonale logischerweise 1,00)
  diag(spearmanMatrix) = NA
  
  #Zuweisung der Namen aller fuer die Simulation benoetigten Matrizen
  assign(paste0("userMatrix", dataSetVar), userMatrix)
  assign(paste0("uDataActiveUser", dataSetVar), uDataActiveUser)
  assign(paste0("spearmanMatrix", dataSetVar), spearmanMatrix)
  
  #Namen in die Vektoren fuern den spaeteren Zugriff eintragen
  userMatrixNames <- c(userMatrixNames, paste0("userMatrix", dataSetVar))
  uDataActiveUserNames <- c(uDataActiveUserNames, paste0("uDataActiveUser", dataSetVar))
  spearmanMatrixNames <- c(spearmanMatrixNames, paste0("spearmanMatrix", dataSetVar))
}




######################## FUNKTIONEN #############################
#### BEWERTUNG
fVResult = function(uDataActiveUser, userMatrix, spearmanMatrix, kValue, userID){
  
  #Erzeugen des Active User Vektors (activeUser) per Aufruf eines
  #Vektors aus der uDataActiveUser, wobei die userID aus den Zeilennamen
  #der Matrix abgeleitet wird (z. B. userID 196 = Zeile 35 in 
  #uDataActiveUser)
  activeUser <- uDataActiveUser[which(rownames(uDataActiveUser) == userID),]
  
  #ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
  #dazu wird zunaechst die zweite Spalte von userMatrix an der aktuellen UserID
  #(UserID aus DataXtab) abgerufen (Vergleich der UserID mit Spalte 1 aus
  #userMatrix um Wert aus Spalte 2 zu erhalten)
  activeUser[userMatrix[which(userMatrix[,1] == userID), 2]] <- NA
  
  #ERSETZT DURCH KNN
  #Abfragen der Items aus spearmanMatrix, die in activeUser mit ueber 3 bewertet wurden 
  #so bekommt man viele Vektoren mit den Korrelationskoeffizienten, die am Ende
  #aufsummiert werden um activeUserSpearman zu erhalten
  #activeUserSpearman <- colSums(spearmanMatrix[which(activeUser > 3),], na.rm = TRUE)
  
  ####KNN
  #Korrelationen aller Items die im aktiven Vektor mit über 3 bewertet sind aus
  #der Spearman-Matrix ziehen und zu einer Matrix zusammenfuegen. 
  ColSumMatrix <- spearmanMatrix[which(activeUser > 3),]
  
  #Schleife zur Berechnung der k nearest neighbours fuer den aktuellen
  #Active User Vektor. 
  for (j in seq_len(nrow(ColSumMatrix))) {
    
    #Ranked und sortiert jede Zeile der Matrix ColSumMatrix, um die
    #1 bis kValue (oben definiert) "besten" ItemIDs zu bekommen, welche
    #mit dem jeweiligen Item am besten korrelieren
    kNNItems <- as.numeric(names(sort(rank(-ColSumMatrix[j,], ties.method = "random", na.last = "keep")))[1:kValue])
    
    #Ersetzt alle Stellen in der Matrix ColSumMatrix durch NA, die nicht
    #in kNNItems aufgeführt sind (also alle Items, die nicht die jeweilig
    #k Nearest-Neighbors sind). So können nun per colSums die Spalten der
    #Matrix aufaddiert werden.
    ColSumMatrix[j,which(!names(ColSumMatrix[j,]) %in% kNNItems)] <- NA
  }
  
  ####/KNN
  
  #Active User Vektor mit der Summe aller Spalten der ColSumMatrix auffüllen
  #(kNN ist damit abgeschlossen). NA-Werte werden bei diesem Zwischenschritt
  #entfernt (Funktion nach Vorgabe J. Knoll).
  #activeUserSpearman <- colSums(ColSumMatrix, na.rm = TRUE)
  activeUserSpearman=apply(ColSumMatrix,2,function(v){if(any(!is.na(v))){sum(v,na.rm=T)}else{NA}})
  
  #Stellen in activeUserSpearman, die in activeUser mit 1-5 bewertet wurden, werden mit
  #NA überschrieben damit die Rangliste korrekt ausgegeben wird (bereits bewertete
  #Items würden die Rangliste verfaelschen)
  activeUserSpearman[which(!is.na(activeUser))] <- NA
  
  #activeUser wird hier nur auf diejenigen Items reduziert, die auch in userMatrix aufgeführt
  #sind (userMatrix in der Zeile UserID i und dann nur die Spalten 2:Ende). Alle Items die nicht
  #in userMatrix fuer diese UserID erscheinen werden auf NA gesetzt, damit die Rangfolge mit den
  #richtigen Bewertungen / Items berechnet wird.
  activeUserSpearman[!(as.numeric(names(activeUserSpearman)) %in% userMatrix[(userMatrix[,1] == userID),2:ncol(userMatrix)])] <- NA
  
  #Erzeugen der Rangliste aus activeUserSpearman.
  activeUserItemRanking <- rank(-activeUserSpearman, na.last = "keep")
  
  #Abfrage des aktuellen Ranges jenes Items, das in userMatrix mit 5 bewertet wurde (Spalte 2).
  #Dieser Wert wird als Ergebnis zurueckgegeben.
  resultUserRank <- as.numeric(activeUserItemRanking[userMatrix[(userMatrix[,1] == userID),2]])
  
  return(resultUserRank)
}

#### ROC Kurven
fROC=function(sumResults,iSize=500){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  sumResults=sumResults[!is.na(sumResults)]
  
  FPR=(sort(sumResults)*((1:length(sumResults))-1))/((iSize*(1:length(sumResults)))-(1:length(sumResults))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  TPR=(1:length(sumResults))/(length(sumResults)) # True Positives / All Relevant
  TPR=c(0,TPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  
  ##########     OUTPUT     ##########
  ##### Zeichnen der ROC-Kurve
  return(cbind(FPR,TPR))
}

fAUC=function(sumResults,iSize=500){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  sumResults=sumResults[!is.na(sumResults)]
  
  FPR=(sort(sumResults)*((1:length(sumResults))-1))/((iSize*(1:length(sumResults)))-(1:length(sumResults))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1)
  
  TPR=(1:length(sumResults))/(length(sumResults)) # True Positives / All Relevant
  TPR=c(0,TPR,1)
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  AUC=sum(diff(FPR[-1])*TPR[-c(1,length(TPR))])
  AUC=round(AUC,4)*100
  return(AUC)
}

fNRR=function(sumResults){
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  NRR=round(sum(is.na(sumResults))/length(sumResults),4)*100
  return(NRR)
}





######################## SIMULATION #############################
#Erzeugen von Matrizen mit allen Namen der Ergebnisvektoren und den
#Werten fuer NRR und AUC, zum spaeteren Zeichnen der Grafiken 
#(Reihenbezeichung = Anzahl Trainingsdatensaetze / 
#Spaltenbezeichnung = Werte des kValue Vektors)
resultsNameMatrix <- matrix(nrow = 5, ncol = length(kValues))
colnames(resultsNameMatrix) <- kValues
rownames(resultsNameMatrix) <- 1:5

#Einlesen aller relevanter Datensaetze
for(dataSetVar in 1:5)
{
  #Berechnung der Werte fuer den aktuellen Datensatz
  for (kValueVar in kValues) {
    
    #Arbeitsvektor leeren
    sumResults <- NULL
    
    #Erzeugen der Werte pro Datensatz und Wert fuer k
    for (userID in as.numeric(rownames(uDataActiveUser))) {
      activeResult <- fVResult(
        get(uDataActiveUserNames[dataSetVar]),
        get(userMatrixNames[dataSetVar]),
        get(spearmanMatrixNames[dataSetVar]),
        kValueVar,
        userID)
      sumResults <- c(sumResults, activeResult)
    }
    
    #Ergebnis umbenennen und ablegen
    assign(paste0("sumResults_t", dataSetVar, "_k", kValueVar), sumResults)
    
    #Name des Ergebnisvektors in resultsNameVector ablegen
    resultsNameMatrix[dataSetVar, which(as.numeric(colnames(resultsNameMatrix)) == kValueVar)] <- paste0("sumResults_t", dataSetVar, "_k", kValueVar)
  }
}
