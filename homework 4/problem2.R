library(tidyverse)    # install.packages("tidyverse")
library(fitdistrplus) # install.packages("fitdistrplus")
library(MASS)
library(cluster)
library(mixtools)

setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 4")
data.dir = 'C:\\Users\\tetteh\\Google Drive\\data miining\\homework 4'
acta = readr::read_csv(file.path(data.dir, 'activity.csv'),
                      col_names=TRUE)   



act <- scale(acta)


################################
#K-MEANS CLUSTERING
###############################

Kmax = 20                                 # maximum K
SSE = numeric(Kmax)                       # initiate SSE vector
for(k in 1:Kmax){
  km = kmeans(act, centers=k, nstart=25)    # use 25 starts
  SSE[k] = km$tot.withinss                # get SSE
}

###determining  the number of clusters

plot(1:Kmax, SSE, type='o', las=1, xlab="K")

# K-Means Cluster Analysis
fit <- kmeans(act, 6) # 6 cluster solution


#plot(fit)
# get cluster means 
aggregate(act,by=list(fit$cluster),FUN=mean)
# append cluster assignment
K_act <- data.frame(acta, fit$cluster)


##save prediction
write.table(fit$cluster, file="C:\\Users\\tetteh\\Google Drive\\data miining\\homework 4\\nelson.txt", 
            row.names=FALSE)




################################
#HIERARCHICAL AGGLOMERATIVE
################################
###function for developing dendrogram
pred <-function(df, link_method,K){
  ##calculate distance, K is the number of clusters
  diss = dist(df, method="euclidean")  # calculate distance
  hc = hclust(diss, method= link_method) 
  hc_est = cutree(hc,k=K) ##cluster prediction
  
}



hc_sing <- pred(acta,"single",6)
hc_comp <- pred(acta,"complete",6)
hc_avg <- pred(acta,"average",6)




#########################################
#COMPARING MODELS
#########################################
##first
table(true=hc_sing , est=fit$cluster)[c (1, 4, 2,3,5,6), ]
table(true=hc_comp , est=fit$cluster)
table(true=hc_avg , est=fit$cluster)

#second
library(mclust)
library(phyclust)
RRand(h1_est, fit$cluster)
RRand(h2_est, fit$cluster)
RRand(h3_est, fit$cluster)
RRand(h1_est, h2_est)
RRand(h1_est, h3_est)
RRand(h3_est, h2_est)

############
#PLOTS
##########