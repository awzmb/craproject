######################## LIBRARIES #############################
library(parallel)



######################## ZUSAMMENFUEHRUNG #############################
#Vektor mit den Namen der resultsNameMatrix erzeugen (wichtig, sofern
#zwischenzeitlich der Vektor kValues geaendert wurde! sonst werden
#nicht die korrekten Namen verwendet)
resultsNameMatrixNames <- as.numeric(colnames(resultsNameMatrix))

#Erzeugen eines Vektors mit den Namen der Ergebnisse zum
#spaeteren Zugriff
sumResultsNameVector <- NULL

#Vektoren mit Laenge = Anzahl Werte fuer k erzeugen
AUCVector <- rep(NA, length(resultsNameMatrixNames))
names(AUCVector) <- colnames(resultsNameMatrix)

NRRVector <- rep(NA, length(resultsNameMatrixNames))
names(NRRVector) <- colnames(resultsNameMatrix)

#Zusammenfuehrung der Ergebnisse per Schleife pro Wert fuer k
for (i in seq_len(ncol(resultsNameMatrix))) {
  
  #Vektor zuruecksetzen
  sumResults <- NULL
  
  #Ergebnisse pro Spalte zusammenfuehren (Vektoren aneinanderhaengen)
  for (j in seq_len(length(resultsNameMatrix[,i]))) {
    sumResults <- c(sumResults, get(resultsNameMatrix[j,i]))
  }
  
  #Ergebnis umbenennen und ablegen
  assign(paste0("sumResults_k", resultsNameMatrixNames[i]), sumResults)
  
  #Hinzufuegen des Ergebnisnamens in den Vektor sumResultsNameVector
  sumResultsNameVector <- c(sumResultsNameVector, paste0("sumResults_k", resultsNameMatrixNames[i]))
  
  #AUC und NRR in die Ergebnisvektoren ueberfuehren
  AUCVector[i] <- fAUC(sumResults)
  NRRVector[i] <- fNRR(sumResults)
}




########################
# Kurven fuer alle k Werte
########################
#Erstellen der ROC-Kurven
plotColors = c("sandybrown", "blue", "green", "tomato", "steelblue", "burlywood3", "magenta3", "black")
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve for k Nearest Neighbor", ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector))) {
  lines(fROC(get(sumResultsNameVector[i])), col=plotColors[i])
}
legend(0.8, 0.9, paste0("k = ", resultsNameMatrixNames), cex=0.8, col=plotColors, lty=1)




########################
# Proportionalitaet AUC / NRR
########################
#Erstellen der ROC-Kurven
plotColors = c("black", "gray75")
plot(NULL, xlim=c(0,20), ylim=c(0,100), main="Verhältnis NRR zu AUC", ylab=" ", xlab=" ")
lines(NRRVector, col=plotColors[1], type = "b")
lines(AUCVector, col=plotColors[2], type = "h")
legend(16, 55, c("NRR", "AUC"), cex=0.8, col=plotColors, lty=1)



########################
# Kurven fuer jeden dritten k Wert (Ausgabe)
########################
#Erstellen der ROC-Kurven
plotColors = c("gray66", "gray66", "gray66", "gray66", "gray5", "gray5", "gray5", "gray5", "gray36",  "gray36", "gray36", "gray36")
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve for k Nearest Neighbor", ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector[seq(1, length(sumResultsNameVector), 3)]))) {
  lines(fROC(get(sumResultsNameVector[seq(1, length(sumResultsNameVector), 3)][i])), col=plotColors[i])
}
legend(0.8, 0.9, paste0("k = ", resultsNameMatrixNames[seq(1, length(resultsNameMatrixNames), 3)]), cex=0.8, col=plotColors, lty=1)





########################
# Kurven fuer alle k Werte
########################
#Erstellen der ROC-Kurven
plotColors = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC Kurve für k mit Abstand von 5% bis zum maximalen Wert", ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector))) {
  lines(fROC(get(sumResultsNameVector[i])), col=plotColors[i])
}








as.numeric(names(AUCVector))


#Erstellen der ROC-Kurven
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(sumResults))
fAUC(sumResults) # in Prozent
fNRR(sumResults) # in Prozent

#for (i in 1:5) {
#  do.call("rbind", )
#}


#Erstellen der ROC-Kurven
plotColors = c("sandybrown", "blue", "green", "tomato", "steelblue", "burlywood3", "magenta3", "black")
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve (T5)", ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (j in seq_len(length(resultsNameMatrix[,i]))) {
  sumResults <- c(sumResults, get(resultsNameMatrix[j,i]))
}
lines(fROC(sumResults_t5_k2), col=plotColors[1])
lines(fROC(sumResults_t5_k3), col=plotColors[2])
lines(fROC(sumResults_t5_k5), col=plotColors[3])
#lines(fROC(sumResults_t5_k10), col=plotColors[4])
#lines(fROC(sumResults_t5_k20), col=plotColors[5])
#lines(fROC(sumResults_t5_k50), col=plotColors[6])
#lines(fROC(sumResults_t5_k100), col=plotColors[7])
lines(fROC(sumResults_t5_k1682), col=plotColors[8])
legend(0.8, 0.5, c("k = 2","k = 3","k = 5","k = 10","k = 20","k = 50","k = 100", "k = MAX"), cex=0.8, col=plotColors, lty=1)




#Ergebnisse zusammenführen
sumResults_k2 <- c(sumResults_t1_k2, sumResults_t2_k2, sumResults_t3_k2, sumResults_t4_k2, sumResults_t5_k2)
sumResults_k3 <- c(sumResults_t1_k3, sumResults_t2_k3, sumResults_t3_k3, sumResults_t4_k3, sumResults_t5_k3)
sumResults_k5 <- c(sumResults_t1_k5, sumResults_t2_k5, sumResults_t3_k5, sumResults_t4_k5, sumResults_t5_k5)
sumResults_k10 <- c(sumResults_t1_k10, sumResults_t2_k10, sumResults_t3_k10, sumResults_t4_k10, sumResults_t5_k10)
sumResults_k20 <- c(sumResults_t1_k20, sumResults_t2_k20, sumResults_t3_k20, sumResults_t4_k20, sumResults_t5_k20)
sumResults_k50 <- c(sumResults_t1_k50, sumResults_t2_k50, sumResults_t3_k50, sumResults_t4_k50, sumResults_t5_k50)
sumResults_k100 <- c(sumResults_t1_k100, sumResults_t2_k100, sumResults_t3_k100, sumResults_t4_k100, sumResults_t5_k100)
sumResults_k1682 <- c(sumResults_t1_k1682, sumResults_t2_k1682, sumResults_t3_k1682, sumResults_t4_k1682, sumResults_t5_k1682)


#Farbgebung fuer die Plots definieren
plotColors = c("sandybrown", "blue", "green", "tomato", "steelblue", "burlywood3", "magenta3", "black")


#Plot ueber Ergebnisse der gesamten Analyse
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve (T5)", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(sumResults_k2), col=plotColors[1])
lines(fROC(sumResults_k3), col=plotColors[2])
lines(fROC(sumResults_k5), col=plotColors[3])
lines(fROC(sumResults_k10), col=plotColors[4])
lines(fROC(sumResults_k20), col=plotColors[5])
lines(fROC(sumResults_k50), col=plotColors[6])
lines(fROC(sumResults_k100), col=plotColors[7])
lines(fROC(sumResults_k1682), col=plotColors[8])
legend(0.8, 0.5, c("k = 2","k = 3","k = 5","k = 10","k = 20","k = 50","k = 100", "k = MAX"), cex=0.8, col=plotColors, lty=1)


#Uebersicht k50 ueber alle Datensaetze
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve (comparison t1 - t5 datasets)", ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(sumResults_t1_k50), col=plotColors[1])
lines(fROC(sumResults_t2_k50), col=plotColors[2])
lines(fROC(sumResults_t3_k50), col=plotColors[3])
lines(fROC(sumResults_t4_k50), col=plotColors[4])
lines(fROC(sumResults_t5_k50), col=plotColors[5])
legend(0.8, 0.5, c("t1", "t2", "t3", "t4", "t5"), cex=0.8, col=plotColors, lty=1)


#Werte fuer AUC und NRR fuer alle k in Matrix schreiben
sumResults_NRR_AUC <- matrix(0, 8, 2)
rownames(sumResults_NRR_AUC) <- c("k2", "k3", "k5", "k10", "k20", "k50", "k100", "kMAX")
colnames(sumResults_NRR_AUC) <- c("NRR", "AUC")
sumResults_NRR_AUC[1,] <- c(fNRR(sumResults_k2), fAUC(sumResults_k2))
sumResults_NRR_AUC[2,] <- c(fNRR(sumResults_k3), fAUC(sumResults_k3))
sumResults_NRR_AUC[3,] <- c(fNRR(sumResults_k5), fAUC(sumResults_k5))
sumResults_NRR_AUC[4,] <- c(fNRR(sumResults_k10), fAUC(sumResults_k10))
sumResults_NRR_AUC[5,] <- c(fNRR(sumResults_k20), fAUC(sumResults_k20))
sumResults_NRR_AUC[6,] <- c(fNRR(sumResults_k50), fAUC(sumResults_k50))
sumResults_NRR_AUC[7,] <- c(fNRR(sumResults_k100), fAUC(sumResults_k100))
sumResults_NRR_AUC[8,] <- c(fNRR(sumResults_k1682), fAUC(sumResults_k1682))


#
plot(NULL, xlim=c(1,2), ylim=c(1,2), main="ROC curve (comparison t1 - t5 datasets)", ylab="NRR", xlab="AUC")
lines(as.numeric(sumResults_NRR_AUC[1:7,1]/sumResults_NRR_AUC[1:7,2]), col=plotColors[1])

#
plot(NULL, xlim=c(0,100), ylim=c(0,100), main="NRR / AUC ratio")
lines(sumResults_NRR_AUC[1:7,1], col=plotColors[1])
lines(sumResults_NRR_AUC[1:7,2], col=plotColors[2])
