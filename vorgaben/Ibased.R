###############################################################################################
###############################################################################################
#####    Ibased.R                                                                         #####
#####    Cusomer Relationship Analytics                                                   #####
###############################################################################################
#####    Technische Hochschule N�rnberg                                                   #####
#####    Fakult�t Informatik                                                              #####
###############################################################################################
#####    Prof. Dr. Rainer Gro�                                                            #####
#####    Julian Knoll (M.A.)                                                              #####
###############################################################################################
###############################################################################################
##### Skript zum item-based collaborative filtering Algorithmus

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


mSimilarityMatrix=cor(mUiMatrix, use = "pairwise.complete.obs", method="spearman")
diag(mSimilarityMatrix)=NA
mSimilarityMatrix

(mSelectedSimilarities=mSimilarityMatrix[which(vActiveUserVector>3),])

(vSummedSimilarities=colSums(mSelectedSimilarities,na.rm = T))

vSelectedItems=vSummedSimilarities
vSelectedItems[which(!is.na(vActiveUserVector))]=NA
vSelectedItems

(vRankingList=rank(-vSelectedItems,na.last="keep"))


