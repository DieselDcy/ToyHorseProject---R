### A Estimate the conjoint model at the individual level

#knitr::opts_chunk$set(echo = TRUE)

rm(list=ls())
getwd()
setwd("/Users/lkc/desktop/Summer/GBA424_AnalyticsDesign/Case4")
filename= "GBA424 - Toy Horse Case Data"
load(paste(filename,".Rdata",sep=""))

##by individual
addcol = cbind(rep(1,nrow(conjointData)),conjointData)
col= addcol[,c(-2,-3,-4)]##add column for constant
samplesize = unique(conjointData$ID)##200 individuals
partworths = matrix(nrow=200, ncol=ncol(col))
for(i in 1:200){ ##for each individual run the regression
  partworths[i,]=lm(ratings~price+size+motion+style,data=conjointData,subset=ID==i)$coef}
atts=c("price","size","motion","style");
colnames(partworths) = c("Intercept",atts)


#Prepare for cluster analysis
toClust<-partworths

##complete the data
missingvalue=conjointData[is.na(conjointData$ratings),]
for(i in 1:200){
  missingvalue[missingvalue$ID==i,3]= partworths[i,1]+partworths[i,2]*missingvalue[missingvalue$ID==i,4]+partworths[i,3]*missingvalue[missingvalue$ID==i,5]+partworths[i,4]*missingvalue[missingvalue$ID==i,6]+partworths[i,5]*missingvalue[missingvalue$ID==i,7]
}
CompleteData = rbind(missingvalue,conjointData[is.na(conjointData$ratings)!=TRUE,])

#Prepare for market simulation
marketsimu = matrix(nrow=200, ncol=16)
for(i in 1:200){
  a = CompleteData[CompleteData$ID==i,c('profile','ratings')]
  a = a[order(a$'profile'),"ratings"]
  marketsimu[i,] = t(a)}
colnames(marketsimu) = c(1:16)

write.csv(marketsimu,"marketsimu.csv",row.names = TRUE)






### B Conduct Benefit Segmentation via Cluster Analysis of Conjoint Part-Utilities

#getwd()
#setwd("/Users/lkc/desktop/Summer/GBA424_AnalyticsDesign/Case4")
#filename= "GBA424 - Toy Horse Case Data"
#load(paste(filename,".Rdata",sep=""))

profilesData
respondentData
#install.packages("data.table")
#install.packages("factoextra")
library(cluster) 
library(fpc)
library(foreign)
library(data.table)
library(factoextra)


d_conjoint <-  conjointData[,2:7]    # select the relevant data for clustering
conjoint_norm <- apply(conjointData,2,function(x) {(x - mean(x))/sd(x)})


d_clust <- toClust


nClust <- 3 ## set number of clusters
set.seed(123456)   # set random number seed before doing cluster analysis
km <- kmeans(na.omit(d_clust),nClust,iter.max = 50, nstart=20)

str(km)

km$centers
ctrs <- data.table(Segment = 1:nrow(km$centers),km$centers,Size = km$size/sum(km$size))
ctrs[order(-Size),]

head(km$cluster)

##pie chart with cluster membership percentages
nc <- length(km$size)
percsize <- paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
pie(km$size,labels=percsize,col=1:nc)


##choosing the number of clusters
## WSS plot - look for "elbow" in curve
fviz_nbclust(na.omit(d_clust),kmeans,method="wss",iter.max=100,nstart=20,k.max=15)

##barplot of the cluster means
axis <- range(km$centers)+(range(km$centers)[2] - range(km$centers)[1])*c(-.1,.1)
bm <- barplot(km$centers,col=1:nClust,beside=TRUE,las=2,main="Cluster Means",ylim = axis)
text(bm,km$centers + .05*ifelse(km$centers>0,1,-1),formatC(km$centers,format="f",digits=1))





### C.Conduct a priori segmentation.

desmat = as.matrix(conjointData[,c(-1,-2,-3)])
mergeData<-merge(conjointData,respondentData,by.x="ID",by.y="ID")


#priori segmentation
summary(lm(ratings~desmat,data = conjointData))
summary(lm(ratings~desmat*age,data = mergeData))#not significant
summary(lm(ratings~desmat*gender,data = mergeData))# significant

summary(lm(ratings~desmat,data = mergeData,subset=gender==1))#price+,size+ motion-
summary(lm(ratings~desmat,data = mergeData,subset=gender==0))#price+,motion- style-




### D.Simulate market shares for different product-line scenarios
getwd()
setwd("/Users/lkc/desktop/Summer/GBA424_AnalyticsDesign/Case4")
filename= "GBA424 - Toy Horse Case Data"
load(paste(filename,".Rdata",sep=""))

# Read data
data = read.csv("marketsimu.csv")[,-1]
# input scenarios
scens = list()
###update all 2 product line
scens[[1]]=c(2,4,7)     
scens[[2]]=c(2,7,10)
scens[[3]]=c(2,7,12)
scens[[4]]=c(2,7,16)  
scens[[5]]=c(4,7,10)
scens[[6]]=c(4,7,12)
scens[[7]]=c(4,7,16)
scens[[8]]=c(7,10,12)
scens[[9]]=c(7,10,16)
scens[[10]]=c(7,12,16)

scens[[11]]=c(5,7,13)#status quo

###update all 2 product line and add a new product line
scens[[12]]=c(2,4,7,10)
scens[[13]]=c(2,4,7,12)
scens[[14]]=c(2,4,7,16) 
scens[[15]]=c(2,7,10,12)

###keep 1 product line and add a new one
scens[[16]]=c(2,5,7)
scens[[17]]=c(4,5,7)
scens[[18]]=c(5,7,10)
scens[[19]]=c(5,7,12)
scens[[20]]=c(5,7,16) 
scens[[21]]=c(2,7,13) 
scens[[22]]=c(4,7,13) 
scens[[23]]=c(7,10,13) 
scens[[24]]=c(7,12,13)
scens[[25]]=c(7,13,16)

###keep 1 product line and add 2 new product lines
scens[[26]]=c(2,4,5,7)
scens[[27]]=c(2,5,7,10)
scens[[28]]=c(2,5,7,12)
scens[[29]]=c(2,5,7,16)
scens[[30]]=c(4,5,7,10)
scens[[31]]=c(4,5,7,12)
scens[[32]]=c(4,5,7,16)
scens[[33]]=c(5,7,10,12)
scens[[34]]=c(5,7,10,16)
scens[[35]]=c(5,7,12,16)
scens[[36]]=c(2,4,7,13)
scens[[37]]=c(2,7,10,13)
scens[[38]]=c(2,7,12,13)
scens[[39]]=c(2,7,13,16)
scens[[40]]=c(4,7,10,13)
scens[[41]]=c(4,7,12,13)
scens[[42]]=c(4,7,13,16)
scens[[43]]=c(7,10,12,13)
scens[[44]]=c(7,10,13,16)
scens[[45]]=c(7,12,13,16)

###keep 1 new product line
scens[[46]]=c(2,7) 
scens[[47]]=c(4,7) 
scens[[48]]=c(7,10) 
scens[[49]]=c(7,12)
scens[[50]]=c(7,16)

###keep both 2 old line and add a new one
scens[[51]]=c(4,5,7,13)
scens[[52]]=c(5,7,10,13)
scens[[53]]=c(5,7,12,13)
scens[[54]]=c(5,7,13,16)



# functions for calculating market share
simScenarios = function(scenarios,data,...){
  res = matrix(nrow=length(scenarios),ncol=length(data)) #sets everything to NA by default
  for(i in 1:length(scenarios)){ ##loop over scenarios
    res[i, scenarios[[i]] ] = simFCShares(scenarios[[i]],data,...)
    ##  calculate market shares and save to right columns in res for the scenario
  }
  res = as.data.frame(res); names(res) = names(data) #setting type and names
  res ##return result table
}

simFCShares = function(scen,data,bestValueIsLow=TRUE){ 
  if(bestValueIsLow==FALSE) { #best value is high
    data = -data  #make values opposite sign so e.g,. 5 become -5 and now finding the min still works.
  }
  inmkt = data[,scen] #construct the subsetted matrix of options
  decs = inmkt
  for (i in 1:200){
    for(j in 1:length(scen)){
      if(decs[i,j]!=inmkt[i,which.min(inmkt[i,])]){
        decs[i,j] = 0
      } else{
        decs[i,j] = 1
      }
      
    }
  }
  shs = colSums(decs)/sum(decs) #assumes that total decisions is market size
  if (length(shs)==length(inmkt)){
    names(shs) = names(inmkt) #attach labels
  }else{
    for (i in 1:length(scen)){
      if(sum(which(bestOpts==i))==0){
        shs[paste("as.factor(bestOpts)",i,sep='')]=0
      }
    }
  }  
  shs
}



##Test the code 
#test the full worker function
#simFCShares(scens[[1]],data,bestValueIsLow=FALSE)
#test the top-level function using the full simFCShares
#simScenarios(scens,data[,1:16],bestValueIsLow=FALSE)


#Calculate short-term profit, long-term profit and competitor's profit
profilesData$vcost = c(21,21,29,29,33,33,41,41,21,21,29,29,33,33,41,41)

simProfit_y1 = function(mktshr,i,myProds,prices,vcosts,fcosts,mktsize) {
  mktshr = mktshr[i,];#the market share distribution in the "i"th scenario
  vprofit = mktshr*(prices-vcosts)*mktsize;#Calculate the gross profit
  if (!(5 %in% myProds) & !(13 %in% myProds)) { #Both of the 2 product lines were changed
    profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*length(myProds)
  } else
    if (((5 %in% myProds) | (13 %in% myProds))&!((5 %in% myProds)&(13 %in% myProds))){ #Only oneof them was changed
      profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*(length(myProds)-1)
    } else { #just add a new product line 
      profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*(length(myProds)-2);
    } 
  return(profit)
}


simProfit_y2 = function(mktshr,i,myProds,prices,vcosts,fcosts,mktsize) {
  mktshr = mktshr[i,];
  vprofit = mktshr*(prices-vcosts)*mktsize;
  if (!(5 %in% myProds) & !(13 %in% myProds)) { 
    profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*length(myProds)
  } else
    if (((5 %in% myProds) | (13 %in% myProds))&!((5 %in% myProds)&(13 %in% myProds))){ 
      profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*(length(myProds)-1)
    } else { 
      profit = sum(vprofit[myProds])-fcosts*length(myProds)-7000*(length(myProds)-2);
    } 
  return(profit)
}


# profit in year 1
profit_y1 = list()
mktshr_y1 = simScenarios(scens,data[,1:16],bestValueIsLow=FALSE)
for (i in 1:length(scens)){
  t = which(scens[[i]]==7)
  myProds = scens[[i]][-t]
  price = profilesData[myProds,'priceLabel']
  vc = profilesData[myProds,'vcost']
  profit_y1[i] = simProfit_y1(mktshr_y1,i,myProds,price,vc,20000,4000)  
}


# profit in year 2
profit_y2 = list()
scens2 = scens
for (i in 1:length(scens)){
  t = which(scens[[i]]==7)
  scens2[[i]][t] = 8
}#the competitor changes the product line from P7 to P8
mktshr_y2 = simScenarios(scens2,data[,1:16],bestValueIsLow=FALSE)
for (i in 1:length(scens2)){
  t = which(scens2[[i]]==8)
  myProds = scens2[[i]][-t]
  price = profilesData[myProds,'priceLabel']
  vc = profilesData[myProds,'vcost']
  profit_y2[i] = simProfit_y2(mktshr_y2,i,myProds,price,vc,20000,4000)  
}


competitor_profit_y1 = mktshr_y1[,7]*(139.99-41)*4000-20000
competitor_profit_y2 = mktshr_y2[,8]*(119.99-41)*4000-20000

max_y1 = profit_y1[[which.max(profit_y1)]]
max_y2 = profit_y2[[which.max(profit_y2)]]




