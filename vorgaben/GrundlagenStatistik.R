###############################################################################################
###############################################################################################
#####    GrundlagenStatistik.R                                                            #####
#####    Cusomer Relationship Analytics                                                   #####
###############################################################################################
#####    Technische Hochschule Nürnberg                                                   #####
#####    Fakultät Informatik                                                              #####
###############################################################################################
#####    Prof. Dr. Rainer Groß                                                            #####
#####    Julian Knoll (M.A.)                                                              #####
###############################################################################################
###############################################################################################
##### Skript zu den Grundlagen der Statistik

###############################################################################################
### VORBEREITUNG ##############################################################################
###############################################################################################

vErfahrung=c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,4,4,6,6,10)

vErfahrung  

length(vErfahrung)


###############################################################################################
### ABS./REL. HAEUFIGKEITEN ###################################################################
###############################################################################################

(abs.haeufigkeit=table(vErfahrung))

(rel.haeufigkeit=table(vErfahrung)/length(vErfahrung))

(rel.sum.haeufigkeit=cumsum(rel.haeufigkeit))


### Balken- und Kreisdiagramme

barplot(abs.haeufigkeit)
plot(abs.haeufigkeit)				

pie(rel.haeufigkeit)  			    
barplot(rel.haeufigkeit)					


###############################################################################################
### LAGEMASSE ##################################################################################
###############################################################################################

### Modus (im R-Standard nicht vorhanden)
modus2=function(x)
{
  # tab = table(x)
  # element = which.max(tab)
  # modus = names(element)
  names(which.max(table(x)))
}

modus2(vErfahrung)


### Median
median2=function(x)
{
   rel.h = table(x)/length(x)
   rel.sum.h = cumsum(rel.h)
   as.numeric(names(rel.h[which.min(rel.sum.h<0.5)]))
}

median2(vErfahrung)
median(vErfahrung)


### Arithemtisches Mittel
mean2=function(x)
{
  sum(x)/length(x)
}

mean2(vErfahrung)
mean(vErfahrung)


###############################################################################################
### UEBUNG LAGEMASSŸE

vBuch = c(1,1,2,1,3)

table(vBuch)

table(vBuch)/length(vBuch)

cumsum(table(vBuch)/length(vBuch))

median2(vBuch)

modus2(vBuch)

mean2(vBuch)


###############################################################################################
### Varianz ###################################################################################
###############################################################################################

### Varianz
Variance=function(x)
{
  mean((x-mean(x))^2)
}

Variance(vErfahrung)

### Standardabweichung

sqrt(Variance(vErfahrung))

###############################################################################################
### UEBUNG STREUUNGSMAÃŸE

Variance(vBuch)

sqrt(Variance(vBuch))


###############################################################################################
### SCHIEFE ###################################################################################
###############################################################################################

Skewness=function(x){
  sum(((x-mean(x))/sqrt(Variance(x)))^3)/length(x)
}

Skewness(vErfahrung)

###############################################################################################
### UEBUNG SCHIEFE

Skewness(vBuch)


###############################################################################################
### ZUSAMMENHANGSMASSE ########################################################################
###############################################################################################

mUI.dense = matrix(c(1,3,2,1,2,
                     1,4,3,5,3,
                     3,4,4,5,4,
                     4,5,4,3,1,
                     4,5,5,4,5,
                     2,5,5,4,2),
                   ncol=6)

mUI.sparse = matrix(c(NA,NA,2,1,2,
                      1,4,3,5,3,
                      NA,NA,4,5,4,
                      NA,5,4,3,1,
                      4,5,5,4,NA,
                      2,5,NA,4,2),
                    ncol=6)


###############################################################################################
### Korrelation nach Bravis-Pearson

cor(mUI.dense[,2],mUI.dense[,5])

cor(mUI.dense,mUI.dense)

cor(mUI.sparse[,2],mUI.sparse[,5],use="pairwise.complete.obs")

cor(mUI.sparse,mUI.sparse,use="pairwise.complete.obs")

###############################################################################################
### Korrelation nach Spearman

cor(mUI.sparse[1,],mUI.sparse[2,],method="spearman",use="pairwise.complete.obs") 


z1=rank(mUI.sparse[1,],na.last="keep")
z2=rank(mUI.sparse[2,],na.last="keep")
m1=mean(z1,na.rm=T)
m2=mean(z2,na.rm=T)
m2=7/3

z3=(z1-m1)
z4=(z2-m2)

z5=(z1-m1)*(z2-m2)
sz5=sum(z5,na.rm=T)

z6=(z1-m1)^2
sz6=sum(z6,na.rm=T)

z7=(z2-m2)^2
z7[4]=NA
sz7=sum(z7,na.rm=T)

sz5/(sqrt(sz6)*sqrt(sz7))

###############################################################################################
### Kosinus Ähnlichkeit

simil.cos=function(x,y){
  vNonNA=(!is.na(x)&!is.na(y))
  x=x[vNonNA]
  y=y[vNonNA]
  x%*%y/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}


simil.cos(mUI.sparse[1,],mUI.sparse[2,])

###############################################################################################
### Jaccard-Koeffizient

mUI.bin=mUI.sparse
mUI.bin[which(!is.na(mUI.bin))]=1
mUI.bin[which(is.na(mUI.bin))]=0

simil.jaccard=function(x,y){
  sum(x&y)/sum(x|y)
}

simil.jaccard(mUI.bin[1,],mUI.bin[2,])


###############################################################################################
### GRUNDLAGEN DER MATRIZENRECHNUNG ###########################################################
###############################################################################################

(mA=matrix(c(3,1,2,4,-1,2),ncol=3))

(vZi=mA[2,])
(vSj=mA[,2])

t(mA)

###############################################################################################
### Addition und Subtraktion

(mB=matrix(c(-5,3,6,2,4,-3),ncol=3))

mA+mB
mB+mA

###############################################################################################
### Multiplikation mit einem Skalar

3*mA
mA*3


###############################################################################################
### Multiplikation zweier Matrizen

(mB=matrix(c(1,4,3,2,2,2,3,5,7,4,3,2),ncol=4))

mA%*%mB
mB%*%mA

