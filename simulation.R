#Importing training datasets
data <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(data) <- c("UserID", "ItemID", "Rating", "Timestamp")

item <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(item) <- c("ItemID", "Name")

t1User <- as.matrix(read.table(file = "ML100k/u1.user.items.test.csv", sep = ";"))
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
for (i in 244) {
  t1Active <- DataXtab[i,]
  
  # ItemID ermitteln die mit 5 bewertet wurde und auf NA gesetzt werden muss
  t1Active[t1User[2, t1User[1,] == i]] <- NA
}

#Testweise Ausführung mit UserID 244 -> danach durch i ersetzen
t1Active <- DataXtab[244,]
t1Active[t1User[2, t1User[1,] == 244]] <- NA


t1DataXtab[,i]




#Vektor mit Bewertungen für User 2 erstellen
#+t1Active <- as.numeric(t1User[2,])


#Vektor für Item an Stelle 2 von t1Active (mit 5 bewertetes Item) anzeigen und über Indizes
#die Bewertungen der NA-Items (Stelle 3-Ende im Vektor) abfragen
t1ActiveRated <- t1Spearman[t1Active[2],]



#Nur die Items auswählen, die eine Bewertung höher als 3 haben
t1SelectedSimilarities <- dataXtab[which(t1Active>3),]

#Aufsummieren der Korrelationskoeffizienten der ausgewählten Items (alle Zeilen von t1SelectedSimilarities)
t1SummedSimilarities = colSums(t1SelectedSimilarities,na.rm = T)

#Separate Matrix aus t1SummedSimilarities erzeugen (zur Bearbeitung)
t1SelectedItems <- t1SummedSimilarities

#Indizes der NA-Stellen aus t1Active auf t1SelectedItems anwenden (Filtern)
t1SelectedItems[which(!is.na(t1Active))] = NA

#Rangliste erzeugen
t1RankingList = rank(-t1SelectedItems, na.last = "keep")

#### ROC Kurven
#Vektor mit "False Positive Rates" erzeugen
FPR = ((sort(t1RankingList) - (1:length(t1RankingList)))/(10-length(t1RankingList)))
FPR = c(0,FPR,1)

#Vektor mit "True Positive Rates" erzeugen
TPR = (1:length(t1RankingList))/length(t1RankingList)
TPR = c(0,TPR,1)

plot(FPR, TPR, ty="s", xlim=c(0,1), ylim=c(0,1), main="ROC curve")
lines(c(0,1),c(0,1),ty="l",lty=2)

sum(diff(FPR[-1])*TPR[-c(1,length(TPR))])

if(length(FPR)<10) cbind(FPR,TPR)
