library(mlbench)
library(glmnet)

#-- Settings
n.train =  1000      # number of training obs
n.test =    400     # number of test obs
K =         10     # number of CV folds
alpha =     1     # glmnet tuning alpha (1 = lasso, 0 = ridge)
M =        100     # number of simulations

#-- Data Generating Function
getData <- function(n) mlbench.friedman1(n, sd=2) # data generating function







#-- Simulations

lambda_m= c()
lambda_1se= c()


set.seed(721)
for(m in 1:M) {
  df.train= getData(n.train)
  df.test= getData(n.test)
  x.train = df.train$x
  y.train= df.train$y
  x.test= df.test$x
  y.test = df.test$y
  
  
  fold = sample(rep(1:K, length=nrow(x.train)))
  
  
  ######
  fit<-  cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, foldid=fold)
  #lambda_m = c(lambda_m,fit$lambda.min)
  #lambda_1se =c(lambda_1se,fit$lambda.1se)
  
  
  pred1 <- predict(fit, s=fit$lambda.min, newx=x.test)
  pred2 <- predict(fit, s=fit$lambda.1se, newx=x.test)
  mse1= mean((pred1- y.test)^2)
  mse2= mean((pred2- y.test)^2)
  
  df2 <- data.frame(lambda_min= fit$lambda.min, mse=mse2)
  df1 <- data.frame(lambda_1se= fit$lambda.1se, mse=mse1)
  
  lambda_m = rbind(lambda_m,df2)
  lambda_1se = rbind(lambda_1se,df1)
}

#lambda_m
#lambda_1se



#comparing the MSE's of the two
compar= c()
for (i in 1:M){
  l= lambda_m$mse[i]> lambda_1se$mse[i]
  compar=c(compar,l)
}
table(compar)




