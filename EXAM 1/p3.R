library(igraphdata)

data(UKfaculty, package='igraphdata')


edg= get.edgelist(UKfaculty)
node= V(UKfaculty)
As = get.adjacency(UKfaculty)
Ds <- diag(apply(As, 1, sum))

Ls= Ds-As
group= get.vertex.attribute(UKfaculty)$Group
mata=as.matrix(eigen(Ls)$vectors)

#Since the 81st eigen value is 0, extracting the lowest 9 vectors will correspond to 72-80
df= mata[,72:80]


#-- Run kmeans for multiple K
Kmax = 9                               # maximum K
SSE = numeric(Kmax)                       # initiate SSE vector
for(k in 1:Kmax){
  km = kmeans(df, centers=k, nstart=25)    # use 25 starts
  SSE[k] = km$tot.withinss                # get SSE
}

#-- Plot results
plot(1:Kmax, SSE, type='o', las=1, xlab="K")
#-- Evaluate to truth (both indicate K=5 isn't bad)
km = kmeans(df, centers=4, nstart=25)  # choose K=6
table(true=group, est=km$cluster)
