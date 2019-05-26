library(AppliedPredictiveModeling)
data(hepatic)

bio$injury <- injury
nfolds <- round(281*0.75)
nfolds

set.seed(12334)
ind <- sample(seq(1,281,by = 1),nfolds)
## ?????????
biotrain <- bio[ind,]
## ?????????
biotest <- bio[-ind,]

## ????????????
biotrainlog <- biotrain
biotestlog <- biotest

## ????????????
class <- unique(injury)
output <- data.frame()







# b. Use logistic regression and the one-vs-all strategy to build a classification model.
for(i in 1:length(unique(injury))){
  biotrainlog$injury <- ifelse(biotrain$injury==class[i],1,0)
  logits <- glm(injury~.,data=biotrainlog, family=binomial(link = "logit"))
  # c. Predict the class for each of the samples in the test set.
  pred <-  predict(logits, biotestlog,type="response")
  predicted.classes <- ifelse(pred > 0.5, "1", "0")
  accuracy <- table(predicted.classes, biotestlog$injury)
  accuracy
  acc <- sum(diag(accuracy))/sum(accuracy)
  output <- rbind(output,data.frame(class=class[i],Accuracy=acc))
  
}

