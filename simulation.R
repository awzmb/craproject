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


# Xtab aus u.data erstellen um daraus t1Active zu ziehen

# (ACHTUNG: Anzahl Columns nicht identisch -> muss abgeschnitten werden)
#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
uDataXtab <- xtabs(Rating ~ UserID + ItemID, uData[(uData[,1]) %in% t1User[,1],])
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(uDataXtab) = "matrix"
#Nullwerte durch NA ersetzen
uDataXtab[uDataXtab == 0] = NA


#t1 Active User Vektor mit gleicher Länge wie t1Spearman erstellen 
#for (i in 110) {
#  t1Active <- DataXtab[i,]
#  
#  # ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss
#  t1Active[t1User[2, t1User[1,] == i]] <- NA
#}




######################## SIMULATION #############################
#Testweise Ausführung mit UserID 110 -> danach durch i ersetzen
t1Active <- uDataXtab[110,]

#ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
#dazu wird zunächst die zweite Spalte von t1User an der aktuellen UserID
#(UserID aus DataXtab) abgerufen (Vergleich der UserID mit Spalte 1 aus
#t1User um Wert aus Spalte 2 zu erhalten)
t1Active[t1User[which(t1User[,1] == 110), 2]] <- NA

#Abfragen der Items aus t1Active, die mit ueber 3 bewertet wurden aus t1Spearman
#so bekommt man viele Vektoren mit den Korrelationskoeffizienten, die am Ende
#aufsummiert werden um t1ActiveSpearman zu erhalten
t1ActiveSpearman <- colSums(t1Spearman[which(t1Active > 3),], na.rm = TRUE)

####KNN
#Matrix aus den 
#t1ColSumMatrix <- t1Spearman[which(t1Active > 3),]
#
#sort(rank(t1ColSumMatrix[1,], ties.method = "random"))
#
#for (j in nrow(t1ColSumMatrix)) {
#  rank()
#}

####/KNN

#t1ActiveSpearman <- colSums(t1ColSumMatrix, na.rm = TRUE)

#Stellen in t1ActiveSpearman, die in t1Active mit 1-5 bewertet wurden, werden mit
#NA überschrieben damit die Rangliste korrekt ausgegeben wird (bereits bewertete
#Items würden die Rangliste verfälschen)
t1ActiveSpearman[which(!is.na(t1Active))] <- NA

#t1Active wird hier nur auf diejenigen Items reduziert, die auch in t1User aufgeführt
#sind (t1User in der Zeile UserID i und dann nur die Spalten 3:Ende). Alle Items die nicht
#in t1User fuer diese UserID erscheinen werden auf NA gesetzt, damit die Rangfolge mit den
#richtigen Bewertungen / Items berechnet wird.
t1ActiveSpearman[!(as.numeric(names(t1ActiveSpearman)) %in% t1User[(t1User[,1] == 110),2:ncol(t1User)])] <- NA

#Erzeugen der Rangliste aus t1ActiveSpearman
t1ActiveRank <- rank(t1ActiveSpearman, na.last = "keep")


#### ROC Kurven
######################## FUNKTIONEN #############################
fROC=function(t1ActiveRank,iSize=500){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  t1ActiveRank=t1ActiveRank[!is.na(t1ActiveRank)]
  
  FPR=(sort(t1ActiveRank)*((1:length(t1ActiveRank))-1))/((iSize*(1:length(t1ActiveRank)))-(1:length(t1ActiveRank))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  TPR=(1:length(t1ActiveRank))/(length(t1ActiveRank)) # True Positives / All Relevant
  TPR=c(0,TPR,1) #ROC-Kurve beginnt bei 0,0 und endet bei 1,1
  
  
  ##########     OUTPUT     ##########
  ##### Zeichnen der ROC-Kurve
  return(cbind(FPR,TPR))
  
}

fAUC=function(t1ActiveRank,iSize=500){
  
  ##########   PROCESSING   ##########
  ##### Berechnung der Koordinatenwerte der ROC-Kurve
  t1ActiveRank=t1ActiveRank[!is.na(t1ActiveRank)]
  
  FPR=(sort(t1ActiveRank)*((1:length(t1ActiveRank))-1))/((iSize*(1:length(t1ActiveRank)))-(1:length(t1ActiveRank))) # False Positives / All Irrelevant
  FPR=c(0,FPR,1)
  
  TPR=(1:length(t1ActiveRank))/(length(t1ActiveRank)) # True Positives / All Relevant
  TPR=c(0,TPR,1)
  
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  AUC=sum(diff(FPR[-1])*TPR[-c(1,length(TPR))])
  AUC=round(AUC,4)*100
  return(AUC)
  
}

fNRR=function(t1ActiveRank){
  
  ##########     OUTPUT     ##########
  ##### Berechnung des AUC-Wertes
  NRR=round(sum(is.na(t1ActiveRank))/length(t1ActiveRank),4)*100
  return(NRR)
  
}

#Vektor mit "False Positive Rates" erzeugen
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(t1ActiveRank))
fAUC(t1ActiveRank) # in Prozent
fNRR(t1ActiveRank) # in Prozent
