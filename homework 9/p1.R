setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 9")


df <- read.csv("linkage_train.csv")
df2 <- read.csv("linkage_test.csv")


df2$LOC <- transform(df2$LOC, id= as.numeric(factor(df2$LOC)))$id
df2$POA <- transform(df2$POA, id= as.numeric(factor(df2$POA)))$id
df2$MOA <- transform(df2$MOA, id= as.numeric(factor(df2$MOA)))$id


df$LOC <- transform(df$LOC, id= as.numeric(factor(df$LOC)))$id
df$POA <- transform(df$POA, id= as.numeric(factor(df$POA)))$id
df$MOA <- transform(df$MOA, id= as.numeric(factor(df$MOA)))$id
df$y <- transform(df$y, id= as.numeric(factor(df$y)))$id



library(glmnet)
library(caret)
library(broom)
library(MASS)
library(tidyverse)

set.seed(200)
n= nrow(df) ##==11
nt = 0.7  ## number of training examples
trainr = sample(1:n, nt*n)
traind = df[trainr,]
testd = df[-trainr,] 

y <- traind$y
x <- traind %>% select(spatial,temporal, tod, dow, LOC, POA, MOA,TIMERANGE) %>% data.matrix()
xtest <- testd %>% select(spatial,temporal, tod, dow, LOC, POA, MOA,TIMERANGE) %>% data.matrix()
ytest <- testd$y

####Ridge regression
lambdas <- 10^seq(3, -2, by = -.1)
mod0 <- cv.glmnet(x, y, alpha = 0)
summary(mod0)
beta.enet0 = coef(mod0, s="lambda.min")
pred0 = predict(mod0, newx = xtest, s="lambda.min")
mse0= mean((ytest-pred0)^2)

prd0 = predict(mod0, newx = as.matrix(df2), s="lambda.min")
prb0= predict(mod0, type="response",s="lambda.min", newx = as.matrix(df2)) 

prob0= predict(mod0, type="response",s="lambda.min", newx = x) 

traind$prob0=prob0


library(broom)



####lasso regression
lambdas <- 10^seq(3, -2, by = -.1)
mod1 <- cv.glmnet(x, y, alpha = 1, lambda = lambdas)
summary(mod1)
beta.enet1 = coef(mod1, s="lambda.min")
pred1 = predict(mod1, newx = xtest, s="lambda.min")
mse1= mean((ytest-pred1)^2)




####Elasticnet regression



####Logistic regression
library(tidyverse)
library(caret)
library(glmnet)
library(pROC)

df$y <- ifelse(df$y==1,1,0)


mylogit <- glm(y~., data = df, family = "binomial")

mod2=cv.glmnet(x, y, family = "binomial", alpha = 0, lambda = NULL)
#mod3=glm(y~x, family = "binomial")
beta.enet2 = coef(mod1, s="lambda.min")
prob2= predict(mod2, type="response",s="lambda.min", newx = x) 
prb2= predict(mod2, type="response",s="lambda.min", newx = as.matrix(df2)) 

traind$prob2=prob2

g2 <- roc(y ~ prob2, data = traind)
g0 <- roc(y ~ prob0, data = traind)


plot(g2) 
lines(g0$specificities, g0$sensitivities, col= "red")
legend("topright", 
       c("Logistics", "Ridge regression"), 
       col=c("black", "red"), 
       lty=1, cex=.8)



try = roc(prb2~prb0)
plot(try)


write.csv(prb2, file = "Akakpo_Alphonse.csv")

####model evaluation















