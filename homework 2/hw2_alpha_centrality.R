library(tidyverse)
library(igraph) # install.packages('igraph') if not installed
library(sand) # install.packages('sand') if not installed
library(igraphdata)#
library(plyr)

#-----------------------------------------------------------------------#
#-- Load Data
#-----------------------------------------------------------------------#

data.dir = ("C:\\Users\\tetteh\\Google Drive\\data miining\\homeowkr 2/")
nodes = read.csv(file.path(data.dir, "transfers_nodes.csv"))
edges = read.csv(file.path(data.dir, "transfers.csv"))

#-- Undirected Graph
g_undir = graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
g_undir
plot(g_undir)

graph_from_da

#-- Directed Graph
g_dir = graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
g_dir
plot(g_dir)


# Using the directed graph, calculate the alpha centrality using s=1n, 
#the vector of all ones. Use ?????{.1,.8}. For each ??, plot the results using color 
#to distinguish between the fraud, non-fraud, and unknown nodes.

cent.alpha = alpha_centrality(g_dir, nodes = V(g_dir), alpha = .1,exo = 1)
cent.alpha
plot(g_dir,
     layout=layout_with_fr(g_dir),
     vertex.size=cent.alpha*12, 
     vertex.label.cex=0.6,
     vertex.color=ifelse(V(g_dir)$fraud ==TRUE,"green","red"))

cent.alpha2 = alpha_centrality(g_dir, nodes = V(g_dir), alpha = .8,exo = 1)
cent.alpha2
plot(g_dir,
     layout=layout_with_fr(g_dir),
     vertex.size=cent.alpha2*3, 
     vertex.label.cex=0.6,
     vertex.color=ifelse(V(g_dir)$fraud ==TRUE,"green","yellow"))

#  Repeat for the undirected graph using only ??=.1.
cent.alpha3 = alpha_centrality(g_undir, nodes = V(g_undir), alpha = .1,exo = 1)
cent.alpha3
plot(g_undir,
     layout=layout_with_fr(g_undir),
     vertex.size=cent.alpha3*10, 
     vertex.label.cex=0.6,
     vertex.color=ifelse(V(g_undir)$fraud ==TRUE,"red","yellow"))

#Why can ??=.8 not be used for the undirected graph

cent.alpha4 = alpha_centrality(g_undir, nodes = V(g_undir), alpha = .23813,exo = 1)
cent.alpha4

#f. Using the directed graph, set e=1 for the known fraudsters, e=0 for the legitimate, 
# and e=.01 for the unknown nodes. You can think of e as proportional to the prior 
#probability that a node is a fraudster. Calculate the alpha centrality (using ??=.8) 
#and make the plot.


exo = ifelse(V(g_dir)$fraud ==TRUE,1,0)
exo[is.na(exo)] <- .01

cent.alpha5 = alpha_centrality(g_dir, nodes = V(g_dir), alpha = 0.8, exo = exo)
plot(g_dir,
     layout=layout_with_fr(g_dir),
     vertex.size=cent.alpha5*5, 
     vertex.label.cex=0.6,
     vertex.color=ifelse(V(g_undir)$fraud ==TRUE,"green","yellow"))
