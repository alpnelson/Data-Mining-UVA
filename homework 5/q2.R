setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 3")
data.dir = 'C:\\Users\\tetteh\\Google Drive\\data miining\\homework 3'
df2 = readr::read_csv(file.path(data.dir, 'crashes16.csv'),
                      col_names=TRUE)   



plot(df2, pch=19, col=cols, 
     xlab="x", ylab="y", las=1, asp=1)
text(X[1:3,], label=1:3, col=2, pos=2)


library(mclust)
mca = Mclust(df2,
            G = NULL,    # set number of non-noise components
            initialization=list(noise=TRUE)) # allow noise/outliers


summary(mca)
plot(mca, what="classification", asp=1, las=1)
grid()
