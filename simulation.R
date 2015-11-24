######################## LIBRARIES #############################
library(parallel)

######################## VARIABLEN #############################
kValue = 2

######################## DATENIMPORT #############################
#Importing training datasets
uData <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(uData) <- c("UserID", "ItemID", "Rating", "Timestamp")

itemDescription <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(itemDescription) <- c("ItemID", "Name")

userMatrix <- as.matrix(read.table(file = "ML100k/u1.user.items.test.csv", sep = ";"))

#Relevante UserIDs
trainingUsers <- read.table(file = "ML100k/u1.user.training.csv", sep = ";")
names(trainingUsers) <- c("UserID")

# Xtab aus u.data erstellen um daraus activeUser zu ziehen
# (ACHTUNG: Anzahl Columns nicht identisch -> muss abgeschnitten werden)
#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
uDataXtab <- xtabs(Rating ~ UserID + ItemID, uData)
#Nullwerte durch NA ersetzen
uDataXtab[uDataXtab == 0] = NA
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(uDataXtab) = "matrix"
uDataActiveUser <- uDataXtab[(rownames(uDataXtab) %in% userMatrix[,1]),]
uDataSpearman <- uDataXtab[(rownames(uDataXtab) %in% trainingUsers[,1]),]
#Ähnlichkeitsmatrix berechnen (Spearman)
spearmanMatrix <- cor(uDataSpearman, use = "pairwise.complete.obs", method="spearman")
#Diagonale der Ähnlichkeitsmatrix durch NA ersetzen (Ähnlichkeit der Diagonale logischerweise 1,00)
diag(spearmanMatrix) = NA


######################## FUNKTIONEN #############################
UserVResult = function(uDataActiveUser, userMatrix, spearmanMatrix, userID){
  
  #Erzeugen des Active User Vektors (activeUser) per Aufruf eines
  #Vektors aus der uDataActiveUser, wobei die userID aus den Zeilennamen
  #der Matrix abgeleitet wird (z. B. userID 196 = Zeile 35 in 
  #uDataActiveUser)
  activeUser <- uDataActiveUser[which(rownames(uDataActiveUser) == userID),]
  
  #ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
  #dazu wird zunächst die zweite Spalte von userMatrix an der aktuellen UserID
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
  #der Spearman-Matrix ziehen und zu einer Matrix zusammenfügen. 
  ColSumMatrix <- spearmanMatrix[which(activeUser > 3),]
  
  #names(sort(rank(-ColSumMatrix[1,], ties.method = "random", na.last = "keep")))[1:5]
  #names(sort(rank(-ColSumMatrix[2,], ties.method = "random", na.last = "keep")))[1:5]
  
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
  #(kNN ist damit abgeschlossen)
  #activeUserSpearman <- colSums(ColSumMatrix, na.rm = TRUE)
  activeUserSpearman=apply(ColSumMatrix,2,function(v){if(any(!is.na(v))){sum(v,na.rm=T)}else{NA}})
  
  #Stellen in activeUserSpearman, die in activeUser mit 1-5 bewertet wurden, werden mit
  #NA überschrieben damit die Rangliste korrekt ausgegeben wird (bereits bewertete
  #Items würden die Rangliste verfälschen)
  activeUserSpearman[which(!is.na(activeUser))] <- NA
  
  #activeUser wird hier nur auf diejenigen Items reduziert, die auch in userMatrix aufgeführt
  #sind (userMatrix in der Zeile UserID i und dann nur die Spalten 2:Ende). Alle Items die nicht
  #in userMatrix fuer diese UserID erscheinen werden auf NA gesetzt, damit die Rangfolge mit den
  #richtigen Bewertungen / Items berechnet wird.
  activeUserSpearman[!(as.numeric(names(activeUserSpearman)) %in% userMatrix[(userMatrix[,1] == userID),2:ncol(userMatrix)])] <- NA
  
  #Erzeugen der Rangliste aus activeUserSpearman
  activeUserItemRanking <- rank(-activeUserSpearman, na.last = "keep")
  
  resultUserRank <- as.numeric(activeUserItemRanking[userMatrix[(userMatrix[,1] == userID),2]])
  
  return(resultUserRank)
}

######################## SIMULATION #############################
sumResults <- NULL
for (userID in as.numeric(rownames(uDataActiveUser))) {
  activeResult <- UserVResult(uDataActiveUser, userMatrix, spearmanMatrix, userID)
  sumResults <- c(sumResults, activeResult)
}

UserVResult(uDataActiveUser, userMatrix, spearmanMatrix, 110)


#activeUserRank <- sumResults[2:nrow(sumResults),2:ncol(sumResults)]
#activeUserRank <- colSums(activeUserRank, na.rm = TRUE)

#### ROC Kurven
######################## FUNKTIONEN #############################
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

#Vektor mit "False Positive Rates" erzeugen
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(sumResults))
fAUC(sumResults) # in Prozent
fNRR(sumResults) # in Prozent
