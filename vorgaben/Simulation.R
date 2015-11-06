###############################################################################################
###############################################################################################
#####    Simulation.R                                                                     #####
#####    Cusomer Relationship Analytics                                                   #####
###############################################################################################
#####    Technische Hochschule Nürnberg                                                   #####
#####    Fakultät Informatik                                                              #####
###############################################################################################
#####    Prof. Dr. Rainer Groß                                                            #####
#####    Julian Knoll (M.A.)                                                              #####
###############################################################################################
###############################################################################################
##### Skript zur Durchführung einer Simulationsstudie


##### Parameter dieser R-Instanz: Welche Simulationsläufe sollen durchgeführt werden?
iStart=1
iEnd=5    # Beispiel: Die fünf ersten Simulationsläufe (aus mSimulations) berechnen


##### Matrix mSimulations: Enthält alle Variationen der Parameter der Simulation
#     Variierende Parameter (pro Parameter eine Spalte)
mSimulations = expand.grid(1:5,1:2,1:4)


##### Funktion fMap: Funktion, die auf die Parameter der Zeile iJob der Matrix mSimulations angewendet wird
fMap=function(iJob){
  
  # Parameter für diesen Simulationslauf laden
  vSimulation=as.numeric(mSimulations[mSimulations[,1]==iJob,])
  
  
  # User-Item-Matrix laden
  mUiMatrix=as.matrix(read.table(paste0("ml-100k/u",vSimulation[2],".base"),sep="\t"))
  mUiMatrix=xtabs(mUiMatrix[,3]~mUiMatrix[,1]+mUiMatrix[,2],mUiMatrix)
  class(mUiMatrix)="matrix"
  mUiMatrix[mUiMatrix==0]=NA
  
  
  # Active-User-Bewertungen laden
  mActiveUserVectors=as.matrix(read.table(paste0("ml-100k/u",vSimulation[2],".test"),sep="\t"))
  # ...
  
  
  # Algorithmus  
  
  
  # Ergebnis verarbeiten
  vResult=log(vSimulation)
  vResult=c(iJob,vResult)
  return(vResult)
  
}


##### Funktion fReduce: Funktion, um Ergebnisse der Funktionen fMap or fReduce zu aggregieren
fReduce=function(Result1,Result2){
  
  # Ergebnisse in Matrix zusammenfügen
  Result=rbind(Result1,Result2)
  
  # Ergebnis zurückgeben
  return(Result)
  
}


##### Simulationslauf anstoßen 
##### (zur Parallelisierung iStart und iEnd anpassen und mehrere R-Instanzen öffnen)
Result=NULL
for(i in iStart:iEnd){
  Result=fReduce(Result,fMap(i))
  cat(".")
}
Result


##### Workspace speichern
save(mSimulations,fMap,fReduce,Result,file=paste0("result",iStart,"-",iEnd,".Rdata"))
