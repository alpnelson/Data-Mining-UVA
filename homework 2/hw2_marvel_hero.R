library(tidyverse)
library(igraph) # install.packages('igraph') if not installed
library(sand) # install.packages('sand') if not installed
library(igraphdata)#
library(plyr)

#-----------------------------------------------------------------------#
#-- Load Data
#-----------------------------------------------------------------------#

data.dir = ("C:\\Users\\tetteh\\Google Drive\\data miining\\homeowkr 2/")
hero = read.csv(file.path(data.dir, "marvel_hero-network.csv"))
hero1 = plyr::count(hero)
names(hero1)[1]<-paste("from")
names(hero1)[2]<-paste("to")
names(hero1)[3]<-paste("weight")



#hero_network=graph.data.frame(hero,directed = FALSE)
hero_network=graph.edgelist(as.matrix(hero[,1:2]),directed=FALSE)
E(hero_network)$weight=as.matrix(hero[,3])
plot(hero_network,edge.width=E(hero_network)$weight/10)

# fast greedy

hero_fg=cluster_fast_greedy(hero_network, merges = TRUE, modularity = TRUE,
                    membership = TRUE, weights = E(hero_network)$weight)
no_community = sizes(hero_fg)
no_community
m=membership(hero_fg)
plot(hero_fg,hero_network)

# bar plot
library(ggplot2)
data = data.frame(no_community)
names(data)[1]<-paste("Community")
names(data)[2]<-paste("heros")



# Calculate the following centrality scores for the network: eigenvector, betweeness, and degree
# degree centrality
deg = degree(hero_network)
cent.deg = deg/sum(deg)
cent.deg

# betweeness centrality
between= centr_betw(hero_network)$res
cent.between = between/sum(between)

# eigenvector centrality
eigen = centr_eigen(hero_network)$vector
cent.eigen = eigen/sum(eigen)

cent = data.frame(cent.deg,cent.between,cent.eigen)
library(dplyr)
cent <- add_rownames(cent, "hero name")
cent %>%
  arrange(-cent.eigen)

#Show the 3 centrality scores, sorted by betweeness centrality, 
#for the three largest communites. How are these three heros important?

m=as.matrix(m)
cent = data.frame(cent,m)

cent_1 = cent %>%
  filter(m == 1)
cent_2 = cent %>%
  filter(m==2)
cent_3 = cent %>%
  filter(m==3)
cent_1 %>% arrange(-cent.between)
cent_2 %>% arrange(-cent.between)
cent_3 %>% arrange(-cent.between)


