library(tidyverse) # loads dplyr, readr, ggplot2, etc
library(arules)
library(arulesViz)

library(readxl)
data.dir = ("C:\\Users\\tetteh\\Google Drive\\data miining\\homwork 1")
df1 = read_excel(file.path(data.dir, "Online Retail.xlsx"))
df2= df1 %>% filter(!is.na(Description))

df3 = df2 %>% distinct(InvoiceNo,Description, .keep_all = TRUE)

N = n_distinct(df3$InvoiceNo)     # Number of transactions
K = n_distinct(df3$Description) # Number of items


tList = split(df3$Description, df3$InvoiceNo)  #transaction list
trans = as(tList, "transactions") # converting transaction list into transaction order

summary(trans) #summary of transaction


itemFr = count(df3, Description, sort=TRUE) %>% mutate(support=n/K)

# plot top 10
itemFr %>% slice(1:10) %>% 
  ggplot(aes(fct_reorder(Description, n), n)) + # order bars by n
  geom_col() +         # barplot
  coord_flip() +       # rotate plot 90 deg
  theme(axis.title.y = element_blank()) # remove y axis title





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


#-- Find all frequent itemsets with support >= .01
fis = apriori(trans,parameter = list(support = .02, target="frequent"))

apriori2df(fis) %>% arrange(-support)




#-- Find all frequent itemsets (s=.01) of length 2 (minlen=2)
fis2 = apriori(trans, 
               parameter = list(support = .02, minlen=2, target="frequent"))

apriori2df(fis2) %>% arrange(-support)  # order by support (largest to smallest)



#-- Find association rules with support>=.001 and confidence>=.50
rules = apriori(trans, 
                parameter = list(support=.02, confidence=.70, 
                                 minlen=2,target="rules"))

apriori2df(rules) %>% arrange(-lift)
apriori2df(rules) %>% arrange(-confidence)

#-- Add other interest measures
apriori2df(rules) %>% 
  mutate(addedValue = interestMeasure(rules, measure="addedValue", trans), 
         PS = interestMeasure(rules, measure="leverage", trans)) %>% 
  arrange(-addedValue)





fis2 = apriori(trans, 
               parameter = list(support = .02, minlen=3, target="frequent"))
apriori2df(fis2) %>% 
  mutate(lift = interestMeasure(fis2, measure="lift", trans)) %>% 
  arrange(-lift)




#-- Find association rules with support>=.001 and confidence>=.50
rules = apriori(trans, 
                parameter = list(support=.02, confidence=.7, 
                                 minlen=1,target="rules"))

apriori2df(rules) %>% arrange(-lift)
apriori2df(rules) %>% arrange(-confidence)

#-- Add other interest measures
apriori2df(rules) %>% 
  mutate(addedValue = interestMeasure(rules, measure="addedValue", trans), 
         PS = interestMeasure(rules, measure="leverage", trans)) %>% 
  arrange(-addedValue)


