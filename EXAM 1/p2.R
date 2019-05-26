library(tidyverse)    # install.packages("tidyverse")
library(fitdistrplus) # install.packages("fitdistrplus")

setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\midterm")
data.dir = 'C:\\Users\\tetteh\\Google Drive\\data miining\\homework 3'
df2 = readr::read_csv(file.path(data.dir, 'crashes16.csv'),
                      col_names=TRUE)   


library(car)
scatterplot(mile ~ time, data= df2)

plot(df2$mile, df2$time, xlab = "Mile", ylab = "Time")

summary(df2$mile)

summary(df2$time)


#---------------------------------------------------------------------------#
#-- KDE 
#---------------------------------------------------------------------------#
library(ks)
library(kdensity)
eval.pts = expand.grid(mile = seq(87, 136, by=.25), time = seq(0, 7, by=1/24))

bd1= Hscv(df2)

kde1= kde(df2, H=bd1,  eval.points = eval.pts, kernel("gaussian"))


bd2= Hscv.diag(df2)
kde2= kde(df2, H=bd2,  eval.points = eval.pts, kernel("epanechnikov"))


#kde3= kde(df2, H=bd2,  eval.points = eval.pts, kernel("gaussian"))

#kde4= kde(df2, H=bd1,  eval.points = eval.pts, kernel("cosine")) 

df3= data.frame(eval.pts, kde1$estimate)

write.csv(df3, file = "Akakpo_Alphonse.csv")












