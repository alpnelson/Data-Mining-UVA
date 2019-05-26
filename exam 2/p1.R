library(MASS)
setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\exam 2")


df_train= read.csv("activity_train.csv")
df_test = read.csv("activity_test.csv")

test_pred = subset(df_test, select= -y)
test_class= df_test$y



mod1= lda(y ~., data = df_train)


v= predict(mod1,test_pred)


c_matrix <- table(test_class, predict(mod1,test_pred)$class)
c_matrix

sum(diag(c_matrix))/sum(c_matrix)
