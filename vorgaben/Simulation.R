###############################################################################################
###############################################################################################
#####    Simulation.R                                                                     #####
#####    Cusomer Relationship Analytics                                                   #####
###############################################################################################
#####    Technische Hochschule N�rnberg                                                   #####
#####    Fakult�t Informatik                                                              #####
###############################################################################################
#####    Prof. Dr. Rainer Gro�                                                            #####
#####    Julian Knoll (M.A.)                                                              #####
###############################################################################################
###############################################################################################
##### Skript zur Durchf�hrung einer Simulationsstudie


##### Parameter dieser R-Instanz: Welche Simulationsl�ufe sollen durchgef�hrt werden?
iStart=1
iEnd=5    # Beispiel: Die f�nf ersten Simulationsl�ufe (aus mSimulations) berechnen


##### Matrix mSimulations: Enth�lt alle Variationen der Parameter der Simulation
#     Variierende Parameter (pro Parameter eine Spalte)
mSimulations = expand.grid(1:5,1:2,1:4)


##### Funktion fMap: Funktion, die auf die Parameter der Zeile iJob der Matrix mSimulations angewendet wird
fMap=function(iJob){
  
  # Parameter f�r diesen Simulationslauf laden
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
  
  # Ergebnisse in Matrix zusammenf�gen
  Result=rbind(Result1,Result2)
  
  # Ergebnis zur�ckgeben
  return(Result)
  
}


##### Simulationslauf ansto�en 
##### (zur Parallelisierung iStart und iEnd anpassen und mehrere R-Instanzen �ffnen)
Result=NULL
for(i in iStart:iEnd){
  Result=fReduce(Result,fMap(i))
  cat(".")
}
Result


##### Workspace speichern
save(mSimulations,fMap,fReduce,Result,file=paste0("result",iStart,"-",iEnd,".Rdata"))
