setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 9")
df <- read.csv("linkage_train.csv")
df2 <- read.csv("linkage_test.csv")

data= subset(df, select= c(spatial, temporal,y))
library(MASS)

kdd= subset(df, select=c(spatial,temporal))
test= subset(df2, select=c(spatial,temporal))

set.seed(200)
n= nrow(data) ##==11
nt = 0.7  ## number of training examples
tr = sample(1:n, nt*n)
trd= data[tr,]
tst = data[-tr,] 



##linear discriminant analysis
mod_lda <- lda(y ~ ., trd, prior = c(1,1)/2)

tf = data.frame(10,7)
colnames(tf)= c("spatial","temporal")
pred1=predict(mod_lda, tf)


##quadratic discriminant analysis
mod_qda <- qda(y ~ ., trd, prior = c(1,1)/2)
pred2=predict(mod_qda, tf)




library(ks)
bd= Hscv(kdd)
bd
kde1= kde(kdd, H= bd, eval.points = test)
write.csv(kde1$estimate, file = "Akakpo_Alphonse.csv")
