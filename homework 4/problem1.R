library(tidyverse)    # install.packages("tidyverse")
library(fitdistrplus) # install.packages("fitdistrplus")
library(MASS)
suppressPackageStartupMessages(library(dendextend))

setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 4")
data.dir = 'C:\\Users\\tetteh\\Google Drive\\data miining\\homework 4'
clus = read.csv(file.path(data.dir, 'clusthw.csv')) 


##question 1
####Run Hierarchical clustering, using Euclidean distance, and two linkage methods (of your choice). Show the resulting dendrograms.



###function for developing dendrogram
dendro <-function(df, link_method){
  ##calculate distance
  diss = dist(clus, method="euclidean")  # calculate distance
  hc = hclust(diss, method= link_method) 
  plot(hc, hang=-1,las=1,main=paste(link_method, "linkage"))
  
}

####function for visualizing the clusters on the dendrogram, where k is the number of clusters desired
dendro_clus <-function(df, link_method,k){
  ##calculate distance
  diss = dist(clus, method="euclidean")  # calculate distance
  hc = hclust(diss, method= link_method) 
  plot(hc, hang=-1,las=1,main=paste(link_method, "linkage"))
  rect.hclust(hc , k = 3, border = 2:6)
  abline(h = 4, col = 'red')
  
}





##question 2
###Estimate K for one of the linkage methods from part a. Explain why you chose that value of K.
#-- Function to calculate the height and plot, setting deafault linkage method to average
height_hclust <- function(X, method="average"){
  diss = dist(clus, method="euclidean")  # calculate distance
  hc = hclust(diss, method= link_method) 
  n=length(hc$height) # get number of merges
  plot(n:1, hc$height, type='o', xlab="K", ylab="height", las=1,
       xlim=c(1, 50))
              
}

height_hclust(df, "complete")


##question 3,Show a scatterplot of the data using colors to denote the K clusters.

Kclus_plot <- function(df, link_method,K){
  diss = dist(clus, method="euclidean")  # calculate distance
  hc = hclust(diss, method= link_method) 
  plot(df, col=cutree(hc, k=K), asp=1, las=1, 
       main=paste(link_method, "K=", K))
  
}

Kclus_plot(clus,"single",6)












########################
#K-CLUSTERS
#######################
dat= scale(clus)
Kmax = 20                                 # maximum K
SSE1 = numeric(Kmax)                       # initiate SSE vector
for(k in 1:Kmax){
  km = kmeans(df, centers=k, nstart=25)    # use 25 starts
  SSE1[k] = km$tot.withinss                # get SSE
}
plot(1:Kmax, SSE1, type='o', las=1, xlab="K")

ft <- kmeans(dat, 4)
plot(clus$x,clus$y, col= fit$cluster)


###Show a scatterplot of the data using colors to denote the K clusters.
K = 4
km = kmeans(dat, centers=K, nstart=25)
plot(dat, col=km$cluster, asp=1, las=1, pch=19,
     main=paste("K-means, K=", K))
