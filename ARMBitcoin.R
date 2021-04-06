
library(tidyverse)
library(tokenizers)


bc<-read.csv("BitcoinNews_2021_1.csv",header = TRUE)


### transfer the text into transaction data
##### here I remove stopwords, punctuation, numbers, make word in lowercase. Every word is in a single column.

bc_title<-bc[,-c(1,2,4)]
bc_title<-as.data.frame(bc_title)

## Start the file
TransactionreviewFile = "bctitle.csv"

Trans <- file(TransactionreviewFile, open = "a")
# for(i in 1:nrow(bc_title)){
#   Tokens<-tokenize_words(bc_title$bc_title[i],stopwords = stopwords::stopwords("en"), 
#                          lowercase = TRUE,  strip_punct = TRUE, simplify = TRUE)
#   cat(unlist(Tokens), "\n", file=Trans, sep=",")
# }

for(i in 1:nrow(bc_title)){
  Tokens<-tokenize_words(bc_title$bc_title[i],
                         lowercase = TRUE,  strip_punct = TRUE, simplify = FALSE)
  cat(unlist(Tokens), "\n", file=Trans, sep=",")
}
close(Trans)
### review the cleaned transaction dataset
bc_title <- read.csv(TransactionreviewFile, 
                     header = FALSE, sep = ",")
head(bc_title)
## Convert all columns to char 
bc_title<-bc_title %>%
  mutate_all(as.character)



# More cleaning, remove digit and remove words with string length shorter than 4 and longer than 9.
MyDF<-NULL
MyDF2<-NULL
for (i in 1:ncol(bc_title)){
  MyList=c() 
  MyList2=c() # each list is a column of logicals ...
  MyList=c(MyList,grepl("[[:digit:]]", bc_title[[i]]))
  MyDF<-cbind(MyDF,MyList)  ## create a logical DF
  MyList2=c(MyList2,(nchar(bc_title[[i]])<4 | nchar(bc_title[[i]])>9))
  MyDF2<-cbind(MyDF2,MyList2) 
  ## TRUE is when a cell has a word that contains digits
}
## For all TRUE, replace with blank
bc_title[MyDF] <- ""
bc_title[MyDF2] <- ""
(head(bc_title,10))

# Now we save the dataframe using the write table command 
write.table(bc_title, file = "UpdatedTBCtitle.csv", col.names = FALSE, 
            row.names = FALSE, sep = ",")

library(tidyverse)
library(viridis)
library(arules)
library(TSP)
library(data.table)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)

# Load the transaction data

ReviewTrans <- read.transactions("/Users/nikkkikong/Desktop/565 Project/Data Cleaning/UpdatedTBCtitle.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
ReviewTrans_rules = arules::apriori(ReviewTrans, 
                                    parameter = list(support=.01, conf=0.01, minlen=2))
inspect(ReviewTrans_rules)

##  SOrt by Conf
SortedRules_conf <- sort(ReviewTrans_rules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf[1:15])
## Sort by Sup
SortedRules_sup <- sort(ReviewTrans_rules, by="support", decreasing=TRUE)
inspect(SortedRules_sup[1:15])
## Sort by Lift
SortedRules_lift <- sort(ReviewTrans_rules, by="lift", decreasing=TRUE)
inspect(SortedRules_lift[1:15])



#### Targeting Bitcoin rules LHS



BCRule <- apriori(ReviewTrans,parameter = list(supp=.01, conf=.01, minlen=2),
                    appearance = list(default="rhs", lhs="bitcoin"),
                    control=list(verbose=FALSE))
BCRule_C <- sort(BCRule, decreasing=TRUE, by="confidence")
BCRule_S <- sort(BCRule, decreasing=TRUE, by="support")
BCRule_L <- sort(BCRule, decreasing=TRUE, by="lift")


inspect(BCRule_C[1:15])
inspect(BCRule_S[1:15])
inspect(BCRule_L[1:15])





plot(BCRule_C[1:15], method="graph", 
     by = 'confidence',control=list(type="itemsets"), 
     main = 'Bitcoin Rules by Confidence', shading = 'confidence')

plot(BCRule_S[1:15], method="graph", 
     by = 'support',control=list(type="itemsets"), 
     main = 'Bitcoin Rules by Support', shading = 'support')

plot(BCRule_L[1:15], method="graph", 
     by = 'lift',control=list(type="itemsets"), 
     main = 'Bitcoin Rules by Lift', shading = 'lift')


## Plot of which items are most frequent
itemFrequencyPlot(ReviewTrans, topN=20, type="absolute",col = rainbow(7),xlab = "Top 20 frequent words", ylab = "Count")+default_theme















