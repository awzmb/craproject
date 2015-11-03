#Importing training datasets
data <- read.table(file = "ML100k/u.data.csv", sep = ";")
names(data) <- c("UserID", "ItemID", "Rating", "Timestamp")

item <- read.table(file = "ML100k/u.item.csv", sep = ";")
names(item) <- c("ItemID", "Name")

t1user <- read.table(file = "ML100k/u1.user.items.test.csv", sep = ";")
names(t1user) <- c("UserID", "Rated5")

#Relevante UserIDs
t1training <- read.table(file = "ML100k/u1.user.training.csv", sep = ";")
names(t1training) <- c("UserID")

#Relevante UserIDs per merge filtern (Verbinden von data mit t1training)
t1data <- data
#t1data <- merge(data, t1training, by = "UserID")
#t1data$Timestamp <- NULL

#xtab erstellen -> Rating als Wert, UserID als y, ItemID als x
t1xtab <- xtabs(Rating ~ UserID + ItemID, t1data)
#Matrix aus xtab erstellen (korrekte Anzeige als Matrix in R)
class(t1xtab) = "matrix"
#Nullwerte durch NA ersetzen
t1xtab[t1xtab==0]=NA

#Vektor mit Bewertungen für User 2 erstellen
t1active <- t1xtab[2,]

#Ähnlichkeitsmatrix berechnen (Spearman)
t1spearman <- cor(t1xtab, use = "pairwise.complete.obs", method="spearman")

#Diagonale der Ähnlichkeitsmatrix durch NA ersetzen (Ähnlichkeit der Diagonale logischerweise 1,00)
diag(t1spearman) = NA
