######################## LIBRARIES #############################
library(parallel)

######################## VARIABLEN #############################
kValue = 3

######################## DATENIMPORT #############################
#Importing training datasets
uData <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(uData) <- c("UserID", "ItemID", "Rating", "Timestamp")

itemDescription <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(itemDescription) <- c("ItemID", "Name")

t1User <- as.matrix(read.table(file = "ML100k/u1.user.items.test.csv", sep = ";"))
#names(t1User) <- c("UserID", "Rated5")
#UserID (erste Spalte) als Rownames verwenden
#rownames(t1User) <- t1User[,1]
#t1User <- t1User[,-1]

#Relevante UserIDs
t1Training <- read.table(file = "ML100k/u1.user.training.csv", sep = ";")
names(t1Training) <- c("UserID")

#Relevante UserIDs per merge filtern (Verbinden von data mit t1Training)
t1Data <- merge(uData, t1Training, by = "UserID")
t1Data$Timestamp <- NULL

#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
t1DataXtab <- xtabs(Rating ~ UserID + ItemID, t1Data)
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(t1DataXtab) = "matrix"
#Nullwerte durch NA ersetzen
t1DataXtab[t1DataXtab == 0] = NA

#Ähnlichkeitsmatrix berechnen (Spearman)
t1Spearman <- cor(t1DataXtab, use = "pairwise.complete.obs", method="spearman")

#Diagonale der Ähnlichkeitsmatrix durch NA ersetzen (Ähnlichkeit der Diagonale logischerweise 1,00)
diag(t1Spearman) = NA

# Xtab aus u.data erstellen um daraus activeUser zu ziehen
# (ACHTUNG: Anzahl Columns nicht identisch -> muss abgeschnitten werden)
#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
uDataXtab <- xtabs(Rating ~ UserID + ItemID, uData[(uData[,1]) %in% t1User[,1],])
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(uDataXtab) = "matrix"
#Nullwerte durch NA ersetzen
uDataXtab[uDataXtab == 0] = NA

#t1 Active User Vektor mit gleicher Länge wie spearmanMatrix erstellen 
#for (i in userID) {
#  activeUser <- DataXtab[i,]
#  
#  # ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss
#  activeUser[t1User[2, t1User[1,] == i]] <- NA
#}

######################## FUNKTIONEN #############################
UserVResult = function(uDataXtab, userMatrix, spearmanMatrix, userID){

  #Erzeugen des Active User Vektors (activeUser) per Aufruf eines
  #Vektors aus der uDataXtab, wobei die userID aus den Zeilennamen
  #der Matrix abgeleitet wird (z. B. userID 196 = Zeile 35 in 
  #uDataXtab)
  activeUser <- uDataXtab[which(rownames(uDataXtab) == userID),]
  
  #ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
  #dazu wird zunächst die zweite Spalte von t1User an der aktuellen UserID
  #(UserID aus DataXtab) abgerufen (Vergleich der UserID mit Spalte 1 aus
  #t1User um Wert aus Spalte 2 zu erhalten)
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
  activeUserSpearman <- colSums(ColSumMatrix, na.rm = TRUE)
  
  #Stellen in activeUserSpearman, die in activeUser mit 1-5 bewertet wurden, werden mit
  #NA überschrieben damit die Rangliste korrekt ausgegeben wird (bereits bewertete
  #Items würden die Rangliste verfälschen)
  activeUserSpearman[which(!is.na(activeUser))] <- NA
  
  #activeUser wird hier nur auf diejenigen Items reduziert, die auch in t1User aufgeführt
  #sind (t1User in der Zeile UserID i und dann nur die Spalten 2:Ende). Alle Items die nicht
  #in t1User fuer diese UserID erscheinen werden auf NA gesetzt, damit die Rangfolge mit den
  #richtigen Bewertungen / Items berechnet wird.
  activeUserSpearman[!(as.numeric(names(activeUserSpearman)) %in% userMatrix[(userMatrix[,1] == userID),2:ncol(userMatrix)])] <- NA
  
  #Erzeugen der Rangliste aus activeUserSpearman
  activeUserRank <- rank(-activeUserSpearman, na.last = "keep")
  
  return(activeUserRank)
}

######################## SIMULATION #############################
uDataXtab = uDataXtab
userMatrix = t1User
spearmanMatrix = t1Spearman

sumResults <- NA
for (userID in as.numeric(rownames(uDataXtab))) {
  activeResult <- c(userID, UserVResult(uDataXtab, userMatrix, spearmanMatrix, userID))
  sumResults <- rbind(sumResults, activeResult)
}

activeUserRank <- sumResults[2:nrow(sumResults),2:ncol(sumResults)]
activeUserRank <- colSums(activeUserRank, na.rm = TRUE)

#### ROC Kurven
######################## FUNKTIONEN #############################
fROC=function(activeUserRank,iSize=length(activeUserRank)){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  activeUserRank=activeUserRank[!is.na(activeUserRank)]
  
  FPR=(sort(activeUserRank)*((1:length(activeUserRank))-1))/((iSize*(1:length(activeUserRank)))-(1:length(activeUserRank))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  TPR=(1:length(activeUserRank))/(length(activeUserRank)) # True Positives / All Relevant
  TPR=c(0,TPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  
  ##########     OUTPUT     ##########
  ##### Zeichnen der ROC-Kurve
  return(cbind(FPR,TPR))
  
}

fAUC=function(activeUserRank,iSize=length(activeUserRank)){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  activeUserRank=activeUserRank[!is.na(activeUserRank)]
  
  FPR=(sort(activeUserRank)*((1:length(activeUserRank))-1))/((iSize*(1:length(activeUserRank)))-(1:length(activeUserRank))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1)
  
  TPR=(1:length(activeUserRank))/(length(activeUserRank)) # True Positives / All Relevant
  TPR=c(0,TPR,1)
  
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  AUC=sum(diff(FPR[-1])*TPR[-c(1,length(TPR))])
  AUC=round(AUC,4)*100
  return(AUC)
  
}

fNRR=function(activeUserRank){
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  NRR=round(sum(is.na(activeUserRank))/length(activeUserRank),4)*100
  return(NRR)
  
}

#Vektor mit "False Positive Rates" erzeugen
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(activeUserRank))
fAUC(activeUserRank) # in Prozent
fNRR(activeUserRank) # in Prozent
