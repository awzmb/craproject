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
plotColors = c("gray10", "gray20", "gray30", "gray40", "gray50", "gray60", "gray65", "black")
plotLty = seq(1, 9)
plotPch = seq(21, 36)
plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector))) {
  lines(fROC(get(sumResultsNameVector[i])), col=plotColors[i], lty=plotLty[i], lwd = 2)
}
legend(0.6, 0.5, paste0("k = ", resultsNameMatrixNames, ", NRR = ", NRRVector), cex=0.8, col=plotColors, lty=plotLty)



########################
# Proportionalitaet AUC / NRR
########################
#Erstellen der ROC-Kurven
plotColors = c("black", "gray70")
plotPch = seq(21, 36)
plot(NULL, xlim=c(0,max(as.numeric(names(NRRVector)))), ylim=c(0,100), ylab="%", xlab="k")
lines(y = NRRVector, x = as.numeric(names(NRRVector)), col=plotColors[1], type = "b", lwd = 2)
lines(y = AUCVector, x = as.numeric(names(AUCVector)), col=plotColors[2], type = "h", lwd = 2)
legend((max(as.numeric(names(NRRVector)))-(max(as.numeric(names(NRRVector))))*0.25), 45, c("NRR", "AUC"), cex=0.8, col=plotColors, lty=1)



########################
# Kurven fuer jeden dritten k Wert (Ausgabe)
########################
#Erstellen der ROC-Kurven
plotColors = c("gray66", "gray66", "gray66", "gray66", "gray5", "gray5", "gray5", "gray5", "gray36",  "gray36", "gray36", "gray36")
plotPch = seq(21, 36)
plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ROC curve for k Nearest Neighbor", ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector[seq(1, length(sumResultsNameVector), 3)]))) {
  lines(fROC(get(sumResultsNameVector[seq(1, length(sumResultsNameVector), 3)][i])), col=plotColors[i], lwd = 2)
}
legend(0.6, 0.4, paste0("k = ", resultsNameMatrixNames[seq(1, length(resultsNameMatrixNames), 3)], ", NRR = ", NRRVector[seq(1, length(resultsNameMatrixNames), 3)]), cex=0.8, col=plotColors, lty=1)



########################
# Kurven fuer alle k Werte in 5% Schritten
########################
#Erstellen der ROC-Kurven
plotColors = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
plotPch = seq(21, 36)
plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="True Postitive Rate", xlab="False Positive Rate")
#Erzeugen der Linien
for (i in seq_len(length(sumResultsNameVector))) {
  lines(fROC(get(sumResultsNameVector[i])), col=plotColors[i], lwd = 2)
}



########################
# Uebersicht k50 ueber alle Datensaetze
########################
plotColors = c("gray20", "gray30", "gray45", "gray60", "black")
plotPch = seq(21, 36)
plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="True Postitive Rate", xlab="False Positive Rate")
lines(fROC(sumResults_t1_k50), col=plotColors[1], lty=plotLty[1], pch = 21 type = "o", lwd = 2)
lines(fROC(sumResults_t2_k50), col=plotColors[2], lty=plotLty[2], pch = 25, type = "o", lwd = 2)
lines(fROC(sumResults_t3_k50), col=plotColors[3], lty=plotLty[3], pch = 23, type = "o", lwd = 2)
lines(fROC(sumResults_t4_k50), col=plotColors[4], lty=plotLty[4], pch = 24, type = "o", lwd = 2)
lines(fROC(sumResults_t5_k50), col=plotColors[5], lty=plotLty[5], pch = 26, type = "o", lwd = 2)
legend(0.8, 0.5, c("T1", "T2", "T3", "T4", "T5"), cex=0.8, col=plotColors, lty=plotLty)