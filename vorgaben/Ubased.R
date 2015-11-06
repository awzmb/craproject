###############################################################################################
###############################################################################################
#####    Ubased.R                                                                         #####
#####    Cusomer Relationship Analytics                                                   #####
###############################################################################################
#####    Technische Hochschule Nürnberg                                                   #####
#####    Fakultät Informatik                                                              #####
###############################################################################################
#####    Prof. Dr. Rainer Groß                                                            #####
#####    Julian Knoll (M.A.)                                                              #####
###############################################################################################
###############################################################################################
##### Skript zum user-based collaborative filtering Algorithmus

mUiMatrix=matrix(c(
  NA, 3, 2, NA, 1, 2,
  1, 4, 2, 5, NA, 4,
  2, NA, 4, 3, 5, NA,
  1, 1, NA, 1, 4, 4,
  2, 3, 4, 1, NA, 2),
nrow=5, byrow=T)
colnames(mUiMatrix)=c("Freundschaft Plus","Star Wars","Titanic","Star Trek","Notting Hill","Eat, Pray, Love")
mUiMatrix

(vActiveUserVector=c(NA, 1, 5, NA, 4, NA))


(vSimilarityVector=cor(vActiveUserVector, t(mUiMatrix), use = "pairwise.complete.obs", method="spearman"))

(mWeightedRatings=mUiMatrix*matrix(vSimilarityVector,nrow=nrow(mUiMatrix),ncol=ncol(mUiMatrix)))

(vMedianWRatings=apply(mWeightedRatings,2,median,na.rm=T))

vSelectedItems=vMedianWRatings
vSelectedItems[which(!is.na(vActiveUserVector))]=NA
vSelectedItems

(mRankingList=rank(-vSelectedItems,na.last="keep"))


