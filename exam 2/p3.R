library(MASS)
setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\exam 2")


df= read.csv("activity_train.csv")
df_t= read.csv("activity_test.csv")

df$y = factor(df$y)
df_t$y = factor(df_t$y)


###train data processing
df_train= df[which(df$y== "4" | df$y== "5"),]
#dfg = subset(df, select  = df$y== "4" | df$y== "5")
df_trainx = subset(df_train, select=-y)
df_trainy = df_train$y
df_trainx = as.matrix(df_trainx)
df_trainy = as.matrix(df_trainy)


###test data processing

df_test = df_t[which(df_t$y== "4" | df_t$y== "5"),]
df_testx = subset(df_test, select=-y)
df_testx = as.matrix(df_testx)
df_testy = df_test$y



#####penalized logistics regression model
set.seed(200)

cv1 <- cv.glmnet(df_trainx, df_trainy, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 1)

#predictionm
pred<-predict(cv1, df_testx,type="response")
pred.class <- ifelse(pred > 0.5, "5", "4")
mat<- table(df_testy, pred.class)
sum(diag(mat))/sum(mat)




####variable selection
coef(cv1)
AllCoef <- coef(cv1,s='lambda.min') 
sel_var <- AllCoef[,1][AllCoef[,1] != 0]
sel_var <- names(sel_var)
sel_var <- sel_var[-1]


###new training data with selected variables
new_train = subset(df_train, select = c("y",sel_var))
new_trainx= subset(new_train, select=-y)

new_trainy = new_train$y

##new testing data with selected variables
new_test = subset(df_test, select = c("y",sel_var))
new_testx = subset(new_test, select=-y)

new_testx = data.frame(new_testx)
new_testy = new_test$y



#####LDA on selected variables
set.seed(200)
mod_lda = lda(y ~., data = new_train)
d= predict(mod_lda,new_testx)
table(new_testy, predict(mod_lda,new_testx)$class)





####non_penalized logistic regression model
set.seed(200)
logit <- glm(y~., data = new_train ,family = "binomial")
pred1<-predict(logit, new_testx,type="response")
pred1.class <- ifelse(pred1 > 0.5, "5", "4")
table(new_testy, pred1.class)
