library(tidyverse)
library(igraph) # install.packages('igraph') if not installed
library(sand) # install.packages('sand') if not installed
library(igraphdata)#
library(plyr)

#-----------------------------------------------------------------------#
#-- Load Data
#-----------------------------------------------------------------------#

data.dir = ("C:\\Users\\tetteh\\Google Drive\\data miining\\midterm")
hero = read.csv(file.path(data.dir, "marvel_association.csv"))


#-- basic stats
n_tran = n_distinct(hero$hero)     # Number of transactions
n_items = n_distinct(hero$comic) # Number of items


apriori2df <- function(x){
  if(class(x) == "itemsets"){
    out = data.frame(items=labels(x), x@quality, stringsAsFactors = FALSE)
  }
  else if(class(x) == "rules"){
    out = data.frame(
      lhs = labels(lhs(x)),
      rhs = labels(rhs(x)),
      x@quality, 
      stringsAsFactors = FALSE)
  }
  else stop("Only works with class of itemsets or rules")
  if(require(dplyr)) tbl_df(out) else out
}


#-- distribution of itemset length
count(hero, comic) %>% 
  ggplot(aes(n)) + geom_bar() + xlab("length of itemset")


count(hero, comic) %>% 
  ggplot(aes(n)) + stat_ecdf() + xlab("length of itemset")


tLi = split(hero$hero,hero$comic)    # get transaction list

#-- get transaction class
tras = as(tLi, "transactions")


summary(tras)

#-- get item counts and support for single itemsets
itemFre = count(hero, hero, sort=TRUE)%>% mutate(support=n/n_tran) 


itemFre %>% slice(1:20) %>% 
  ggplot(aes(fct_reorder(hero, n), n)) + # order bars by n
  geom_col() +         # barplot
  coord_flip() +       # rotate plot 90 deg
  theme(axis.title.y = element_blank()) # remove y axis title




#-- Find all frequent itemsets with support >= .01
fi = apriori(tras, 
              parameter = list(support = .01, target="frequent"))

apriori2df(fi) %>% arrange(-support)  # order by support (largest to smallest)

#-- Find all frequent itemsets (s=.01) of length 2 (minlen=2)
fi2 = apriori(tras, 
               parameter = list(support = .01, minlen=2, target="frequent"))

apriori2df(fi2) %>% arrange(-support)  # order by support (largest to smallest)


apriori2df(fi2) %>% 
  mutate(lift = interestMeasure(fi2, measure="lift", tras)) %>% 
  arrange(-lift)


itemst = c("INVISIBLE WOMAN", "THING")
apa = apriori(tras, 
             parameter = list(support=0, target="frequent"), 
             appearance = list(items = itemst))
apriori2df(apa) %>% 
  mutate(lift = interestMeasure(apa, measure="lift", tras)) %>% 
  arrange(-lift)


#-- Find all rules with 'MR. FANTASTIC, THING, INVISIBLE WOMAN, and HUMAN TORCH' on the lhs 
rulz2 = apriori(tras, 
                 parameter = list(support=0, confidence=0.001, 
                                  minlen=5,target="rules"), 
                 appearance = list(lhs = c("INVISIBLE WOMAN", "THING", "MR. FANTASTIC","HUMAN TORCH")))

ok=apriori2df(rulz2) %>% 
  mutate(addedValue = interestMeasure(rulz2, measure="addedValue", trans), 
         PS = interestMeasure(rulz2, measure="leverage", trans)) %>% 
  arrange(-confidence)
View(ok)









min <-count(hero, comic)
h1 = hpi(min$n)

kd <- kde(min$n, H=h1)
plot(kd, main="distribution of the number of items per transaction")


#-- Histogram settings
bw = 10                     # binwidth parameter
bks = seq(40, 110, by=bw)   # create a sequence of numbers
#-- Density Histogram
hist(min$n, freq=FALSE, breaks=bks, las=1, main="Density Histogram")
