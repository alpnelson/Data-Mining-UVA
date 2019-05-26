setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\exam 2")


df <- read.csv("linkage_train.csv")
df2 <- read.csv("linkage_test.csv")


df2$LOC <- transform(df2$LOC, id= as.numeric(factor(df2$LOC)))$id
df2$POA <- transform(df2$POA, id= as.numeric(factor(df2$POA)))$id
df2$MOA <- transform(df2$MOA, id= as.numeric(factor(df2$MOA)))$id


df$LOC <- transform(df$LOC, id= as.numeric(factor(df$LOC)))$id
df$POA <- transform(df$POA, id= as.numeric(factor(df$POA)))$id
df$MOA <- transform(df$MOA, id= as.numeric(factor(df$MOA)))$id
df$y <- transform(df$y, id= as.numeric(factor(df$y)))$id


findpi <- function(x_new,beta){
  pi <- 1:nrow(x_new)
  expon <- 1:nrow(x_new)
  for (i in 1:nrow(x_new)){
    expon[i] <- 0
    for (j in 1:ncol(x_new)){
      expo <- x_new[i,j] * beta[j]
      expon[i] <- expo + expon[i]}
    pi[i] <- exp(expon[i])/(1+exp(expon[i]))
  }
  return(pi)
}





logistic <- function(x,y,learningrate,dif) {
  varia <- length(x)
  beta <- rep(0, (varia+1))
  bias <- rep(1, dim(x)[1])  #intercept
  x_bia <- cbind(bias,x)
  derivative <- 1:(varia+1)
  diff <- 10000
  
  ##predictions
  findpi <- function(x_bia,beta){
    pi <- 1:nrow(x_bia)
    expon <- 1:nrow(x_bia)
    for (i in 1:nrow(x_bia)){
      expon[i] <- 0
      for (j in 1:ncol(x_bia)){
        expo <- x_bia[i,j] * beta[j]
        expon[i] <- expo + expon[i]}
      pi[i] <- exp(expon[i])/(1+exp(expon[i]))
    }
    return(pi)
  }
  
  
  
  
  
  while(diff > dif) {
    pi <- findpi(x_new,beta)
    pi <- as.vector(pi)
    W <- findW(pi)
    derivative <- (solve(t(x_new)%*%W%*%as.matrix(x_new))) %*% (t(x_new)%*%(y - pi))  #hessian
    beta = beta + derivative
    diff <- sum(derivative^2)
    ll <- calculatelikeli(y,pi)  ##likelihood
    print(ll)
  }
  return(beta)
}



mod1 <- logistic(xa,ya,0.01,0.0000005)