#-- Install Required Packages
library(mclust)
library(tidyverse)   
library(readxl)


setwd("C:\\Users\\tetteh\\Google Drive\\data miining\\homework 5")
data.dir = 'C:\\Users\\tetteh\\Google Drive\\data miining\\homework 5'
res = read.csv(file.path(data.dir, 'research_payments.csv'))  


payments= res$payment


extract_first_digit <- function(x){
  d = stringr::str_extract(x, "[1-9]") # get first natural number
  factor(d, levels=1:9)                # convert to factor with levels 1:9
}

dbenford <- function(x) log10(1+1/x)


rbenford <- function(n, digits=1, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  x = 10^(digits-1):(10^digits - 1)
  p = dbenford(x)
  sample(x, size=n, replace=TRUE, prob=p)
}
first_digit = extract_first_digit(payments)

table(first_digit)


Ya = table(first_digit) %>% as.integer
no = length(first_digit)    # number of observations
Ex = no*dbenford(1:9)  # expected count vector
chis = (Ya-Ex)/sqrt(Ex)  # vector of deviations
(chisqa = sum(chis^2)) # chi-squared test statistic


#-- Alternative R function chisq.test()
csqa = chisq.test(Ya, p=dbenford(1:9))
csqa
csqa$statistic    # test statistic value
csqa$p.value      # estimated p-value
csqa$residuals    # residuals are the chi from above






#-- Monte Carlo Testing
M = 10000                           # number of simulations
stat.chisqa = stat.llra = numeric(M) # initialize statistics
for(m in 1:M){
  #- generate observation under the null of Benford
  y.sima = rmultinom(1, size=no, prob=dbenford(1:9)) 
  #- calculate statistics
  stat.chisqa[m] = chisq.test(y.sima, p=dbenford(1:9))$statistic
  pa.mle = y.sima/sum(y.sima)
  stat.llra[m] = dmultinom(y.sima, prob=pa.mle, log=TRUE) - 
    dmultinom(y.sima, prob=dbenford(1:9), log=TRUE)
}


#- calculate p-values
(1 + sum(stat.chisqa > chisqa)) / (M+1)  # chi-square p-value
(1 + sum(stat.llra > llra.mle)) / (M+1)  # LLR p-value



cor(stat.chisqa, stat.llra)  # strong correlation between metrics

#- plot
par(mfrow=c(1,2))
plot(density(stat.chisqa))   # kde plot
abline(v = chisqa, col="red")

plot(ecdf(stat.chisqa))      # ecdf plot
abline(v = chisqa, col="red")

plot(density(stat.llra))
abline(v = llra.mle, col="red")

plot(ecdf(stat.llra))      # ecdf plot
abline(v = llra.mle, col="red")























# MLE
La.mle = dmultinom(Ya, prob=Ya/sum(Ya), log=TRUE)
La.null = dmultinom(Ya, prob=dbenford(1:9), log=TRUE)
llra.mle = La.mle - La.null   # log-likelihood ratio

# discrete uniform
La.dunif = dmultinom(Ya, prob=rep(1/9, 9), log=TRUE)
llr.dunif = La.dunif - La.null

## 2*llr should be close to chi-squared
c(llr = 2*llra.mle, chi.sq=chisqa)


#-- chi-squared p-value
1-pchisq(chisqa, df=8, lower.tail=TRUE )

#-- llr p-value
1-pchisq(2*llra.mle, df=8, lower.tail=TRUE )

