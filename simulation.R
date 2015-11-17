################## FUNKTIONEN
kNearestNeighbors <- function(i, DistanceMatrix, k)
{
  #Ordnen der jeweiligen Spalte (symbolisiert die ItemIDs) der Distanzmatrix
  #um die IDs der jeweils k nächsten Nachbarn zu erhalten.
  OrderedNeighbors <- order(DistanceMatrix[i, ])
  
  #Von der Funktion sollen nur die zweite bis k+1 Stelle zurückgegeben werden,
  #da das Item sich selbst natürlich am nächsten ist.
  return(OrderedNeighbors[2:(k + 1)])
}


#Importing training datasets
data <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(data) <- c("UserID", "ItemID", "Rating", "Timestamp")

item <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(item) <- c("ItemID", "Name")

t1User <- as.matrix(read.table(file = "ML100k/u1.user.items.test.csv", sep = ";"), dimnames(NULL))
#names(t1User) <- c("UserID", "Rated5")
#UserID (erste Spalte) als Rownames verwenden
#rownames(t1User) <- t1User[,1]
#t1User <- t1User[,-1]

#Relevante UserIDs
t1Training <- read.table(file = "ML100k/u1.user.training.csv", sep = ";")
names(t1Training) <- c("UserID")

#Relevante UserIDs per merge filtern (Verbinden von data mit t1Training)
t1Data <- merge(data, t1Training, by = "UserID")
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
DataXtab <- xtabs(Rating ~ UserID + ItemID, data)
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(DataXtab) = "matrix"
#Nullwerte durch NA ersetzen
DataXtab[DataXtab == 0] = NA

#Vektor manuell erzeugen
#t1Active <- vector(mode = "numeric", length = ncol(t1Spearman))
#t1Active[t1Active == 0] <- NA

#t1 Active User Vektor mit gleicher Länge wie t1Spearman erstellen 
#for (i in 244) {
#  t1Active <- DataXtab[i,]
#  
#  # ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss
#  t1Active[t1User[2, t1User[1,] == i]] <- NA
#}

#Testweise Ausführung mit UserID 244 -> danach durch i ersetzen
t1Active <- DataXtab[244,]

#ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss,
#dazu wird zunächst die zweite Spalte von t1User an der aktuellen UserID
#(UserID aus DataXtab) abgerufen (Vergleich der UserID mit Spalte 1 aus
#t1User um Wert aus Spalte 2 zu erhalten)
t1Active[t1User[which(t1User[,1] == 244), 2]] <- NA

#Abfragen der Items aus t1Active, die mit ueber 3 bewertet wurden aus t1Spearman
#so bekommt man viele Vektoren mit den Korrelationskoeffizienten, die am Ende
#aufsummiert werden um t1ActiveSpearman zu erhalten
#t1ActiveSpearman <- colSums(t1Spearman[which(t1Active > 3),], na.rm = TRUE)

################ KNN
#Abfragen der Items aus t1Active, die mit ueber 3 bewertet wurden aus t1Spearman
#so bekommt man viele Vektoren mit den Korrelationskoeffizienten. Daraus wird
#eine euklidische Distanzmatrix erstellt, um kNN anzuwenden. Matrix(-ausschnitt)
#aus t1Spearman muss vorher transponiert werden, da die Funktion 'dist' die
#Werte für die Reihen berechnet.
t1Distances <- as.matrix(dist(t(t1Spearman[which(t1Active > 3),]), method="euclidean"))

for (j in vector) {
  
}
kNearestNeighbors(which(t1Active > 3), t1Distances, 7)


+#Stellen in t1ActiveSpearman, die in t1Active mit 1-5 bewertet wurden, werden mit
#NA überschrieben damit die Rangliste korrekt ausgegeben wird (bereits bewertete
#Items würden die Rangliste verfälschen)
t1ActiveSpearman[which(!is.na(t1Active))] <- NA

#Erzeugen der Rangliste aus t1ActiveSpearman
t1ActiveRank <- rank(t1ActiveSpearman, na.last = "keep")


#Distanzmatrix für kNN erzeugen




#### ROC Kurven
#Vektor mit "False Positive Rates" erzeugen
FPR = ((sort(t1ActiveRank) - (1:length(t1ActiveRank)))/(10-length(t1ActiveRank)))
FPR = c(0,FPR,1)

#Vektor mit "True Positive Rates" erzeugen
TPR = (1:length(t1ActiveRank))/length(t1ActiveRank)
TPR = c(0,TPR,1)

#Ausgabe der ROC-Kurve
plot(FPR, TPR, ty="s", xlim=c(0,1), ylim=c(0,1), main="ROC curve")
lines(c(0,1),c(0,1),ty="l",lty=2)

sum(diff(FPR[-1])*TPR[-c(1,length(TPR))])

if(length(FPR)<10) cbind(FPR,TPR)
