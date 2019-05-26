library(MASS)
setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\exam 2")


df_train= read.csv("activity_train.csv")
df_test = read.csv("activity_test.csv")

df_train$y = factor(df_train$y)
df_test$y = factor(df_test$y)

df_trainx = subset(df_train, select= -y)
df_trainy= df_train$y


df_testx = subset(df_test, select= -y)
df_testy= df_test$y


library(glmnet)

set.seed(333)
claz <- c(1,2,3,4,5,6)
output <- data.frame()
lambdas <- data.frame()




for(i in 1:length(claz)){
  trainx <- df_trainx
  trainy <- df_trainy
  testx <- df_testx
  testy <- df_testy
  
  
  trainy <- ifelse(trainy==claz[i],1,0)
  df_testy<- ifelse(testy==claz[i],1,0)
  logits <- cv.glmnet(as.matrix(trainx),trainy, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 1)
  
  pred <-  predict(logits, as.matrix(testx),type="response")
  predicted.classes <- ifelse(pred > 0.5, "1", "0")
  accuracy <- table(predicted.classes,df_testy)
  accuracy
  acc <- sum(diag(accuracy))/sum(accuracy)
  output <- rbind(output,data.frame(class=claz[i],Accuracy=acc))
  lambdas <- rbind(lambdas,data.frame(class=claz[i], lambda = logits$lambda.min) )
}









