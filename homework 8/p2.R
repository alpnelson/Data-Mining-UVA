setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 8")


data= read.csv("realestate-train.csv")
testd= read.csv("realestate-test.csv")



##dummy coding
#1. CentralAir 2. BldgType 3.HouseStyle
data$CentralAir <- transform(data$CentralAir,id=as.numeric(factor(data$CentralAir)))$id
data$BldgType <- transform(data$BldgType,id=as.numeric(factor(data$BldgType)))$id
data$HouseStyle <- transform(data$HouseStyle,id=as.numeric(data$HouseStyle))$id
#data$condition <- as.factor(data$condition)

###dummy coding for Test data
testd$CentralAir <- transform(testd$CentralAir,id=as.numeric(factor(testd$CentralAir)))$id
testd$BldgType <- transform(testd$BldgType,id=as.numeric(factor(testd$BldgType)))$id
testd$HouseStyle <- transform(testd$HouseStyle,id=as.numeric(testd$HouseStyle))$id

library(glmnet)
x= subset(data, select= -price)  #independdent variables
y= data$price          ##dependent variable


set.seed(25)
n= nrow(data) ##==11
nt = 0.7  ## number of training examples
trainr = sample(1:n, nt*n)
xtrain= x[trainr,]
xtrain= as.matrix(xtrain)
x.test= x[-trainr,]
x.test= as.matrix(x.test)

ytrain= y[trainr]

ytest= y[-trainr]
ytest=as.matrix(ytest)

###elastic net regression

#-- Elastic Net
a = .5  # set alpha for elastic net
mod = cv.glmnet(xtrain, ytrain, alpha=a)
summary(mod)
beta.enet = coef(mod, s="lambda.min")
pred = predict(mod, newx = x.test, s="lambda.min")

mse= mean((ytest- pred)^2)


##building models with different values of alpha
lst <- list()
for (i in 0:10){
  fitnam <- paste0("alpha",i/10)
  lst[[fitnam]]<- cv.glmnet(xtrain, ytrain, alpha=i/10, family="gaussian")
}

lst.mse <- data.frame()
for (i in 1:10){
  fitnam <- paste0("alpha",i/10)
  predy= predict(lst[[fitnam]], newx = x.test, s="lambda.min")
  rmse <-sqrt(mean((ytest- predy)^2))
  df <- data.frame(alpha=i/10, rmse=rmse, fit.name=fitnam)
  
  lst.mse= rbind(lst.mse, df)
}

View(lst.mse)
###prediction with alpha=0.4
predt= predict(lst$alpha0.4, newx = as.matrix(testd) , s="lambda.min")
names(predt)= c("yhat")

write.csv(predt, file = "Akakpo_alphonse.csv")


K=10
fold1 = sample(rep(1:K, length=nrow(xtrain)))
#-- Lasso
fit.lasso = cv.glmnet(xtrain, ytrain, alpha=1, foldid=fold1)
beta.lasso = coef(fit.lasso, s="lambda.min")
yhat.lasso = predict(fit.lasso, newx = x.test, s="lambda.min")



