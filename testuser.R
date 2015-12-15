######################## LIBRARIES #############################
library(parallel)



######################## VARIABLEN #############################
#Wert fuer kNN (Anzahl an Neighbours)
kValues = c(2, 3, 5, 10, 20, 50, 100, 1682)



######################## AUSWERTUNG #############################
for (kValue in kValues) {

    #Erzeugen des Active User Vektors (activeUser) per Aufruf eines
  #Vektors aus der uDataActiveUser, wobei die 765 aus den Zeilennamen
  #der Matrix abgeleitet wird (z. B. 765 196 = Zeile 35 in 
  #uDataActiveUser)
  activeUser <- uDataActiveUser1[which(rownames(uDataActiveUser1) == 765),]
  
  #ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
  #dazu wird zunaechst die zweite Spalte von userMatrix an der aktuellen UserID
  #(UserID aus DataXtab) abgerufen (Vergleich der UserID mit Spalte 1 aus
  #userMatrix um Wert aus Spalte 2 zu erhalten)
  activeUser[userMatrix1[which(userMatrix1[,1] == 765), 2]] <- NA
  
  #ERSETZT DURCH KNN
  #Abfragen der Items aus spearmanMatrix, die in activeUser mit ueber 3 bewertet wurden 
  #so bekommt man viele Vektoren mit den Korrelationskoeffizienten, die am Ende
  #aufsummiert werden um activeUserSpearman zu erhalten
  #activeUserSpearman <- colSums(spearmanMatrix[which(activeUser > 3),], na.rm = TRUE)
  
  ####KNN
  #Korrelationen aller Items die im aktiven Vektor mit über 3 bewertet sind aus
  #der Spearman-Matrix ziehen und zu einer Matrix zusammenfuegen. 
  ColSumMatrix <- spearmanMatrix1[which(activeUser > 3),]
  
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
  activeUserSpearman[!(as.numeric(names(activeUserSpearman)) %in% userMatrix1[(userMatrix1[,1] == 765),2:ncol(userMatrix1)])] <- NA
  
  #Erzeugen der Rangliste aus activeUserSpearman.
  activeUserItemRanking <- as.numeric(names(sort(rank(-activeUserSpearman, na.last = "keep"))[1:10]))
  
  assign(paste0("testUserResults_t1_u765_k", kValue), activeUserItemRanking)
}


#Matrix mit Ergebnissen fuer verschiedene k erzeugen
testUserResults_t1_u765 <- matrix(0, 8, 10)
testUserResults_t1_u765[1,] <- testUserResults_t1_u765_k2
testUserResults_t1_u765[2,] <- testUserResults_t1_u765_k3
testUserResults_t1_u765[3,] <- testUserResults_t1_u765_k5
testUserResults_t1_u765[4,] <- testUserResults_t1_u765_k10
testUserResults_t1_u765[5,] <- testUserResults_t1_u765_k20
testUserResults_t1_u765[6,] <- testUserResults_t1_u765_k50
testUserResults_t1_u765[7,] <- testUserResults_t1_u765_k100
testUserResults_t1_u765[8,] <- testUserResults_t1_u765_k1682
rownames(testUserResults_t1_u765) <- c("k2", "k3", "k5", "k10", "k20", "k50", "k100", "kMAX")


#Rated 5 Item fuer User 765
as.numeric(userMatrix[which(userMatrix1[,1] == 765),2])


#Namen in Matrix schreiben
testUserResults_t1_u765_Names <- matrix(0, 8, 10)

for (i in seq_len(ncol(testUserResults_t1_u765_Names))) {
  testUserResults_t1_u765_Names[1,i] <- itemDescription[testUserResults_t1_u765[1,i],2]
}

itemDescription[testUserResults_t1_u765[1,1],2]
