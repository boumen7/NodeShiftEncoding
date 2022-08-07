####################################################################
# This is the core function of the NSE representation presented in:
# Boulif M. & Gharbi A., A new node-shift encoding representation for 
# the travelling salesman problem, The 1st International Conference 
# on Logistics (ICL 2022), Jeddah, Kingdom of Saudi Arabia, 2022
####################################################################
# An example
guide <- c(1,3,5,4,2,6,7,8,9,10)  
n <- length(guide)
chromo <- sample(0:(n-2),replace = TRUE)
####################################################################
## Normalize tour: brings city 1 to the beginning   ################
####################################################################
normalizeTour <- function(x){
  if(x[1]==1)return(x)
  firstNodeIndex <- match(1,x)
  len <- length(x)
  y <- numeric(len)
  y[1] <-1; j <- 2
  if(firstNodeIndex<len)
    for(i in (firstNodeIndex+1):len) {y[j] <- x[i];j <- j+1}
  
  for(i in 1:(firstNodeIndex-1)) {y[j] <- x[i];j <- j+1}
  return(y)
}
####################################################################
## get the tour associated to an NSE chromosome ####################
####################################################################
getShiftedTour <- function(temoin,chromo){
  len <- length(temoin)
  vRank <- 1:len
  for(i in 2:len){
    oldMigrantRank <- vRank[i]
    newMigrantRank <- vRank[i]+chromo[i-1]
    if(newMigrantRank>len) newMigrantRank <- newMigrantRank-len+1
    if(newMigrantRank>oldMigrantRank){
      for(j in 1:len)
        if((vRank[j]<=newMigrantRank)&&(vRank[j]>=oldMigrantRank))
          vRank[j] <- vRank[j]-1
    }else{
      for(j in 1:len)
        if((vRank[j] < oldMigrantRank)&&(vRank[j]>=newMigrantRank))
          vRank[j] <- vRank[j]+1
    }
    vRank[i]<-newMigrantRank
  }
  tour <- numeric(len)
  for(i in 1:len)
    tour[vRank[i]]<-temoin[i]
  #tour <- normalizeTour(tour) #useless, node 1 non movable
  return(tour)
}
###################################################################
tour <- getShiftedTour(guide,chromo)
print(guide)
print(chromo)
print(tour)
