logsum <- function(x,y){
  sam=0
  for (i in 1:length(x)){
    sam= sam+log(x[i]/y[i])
  }
  return(sam)
}


x = c(1,2,3)
y = c(2,3,4)
