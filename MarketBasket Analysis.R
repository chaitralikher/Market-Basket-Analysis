#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)


#read data file into R dataframe
#setwd("/Volumes/Chaitrali/")
basket <- read_csv("BreadBasket_DMS.csv")


basket <- basket[complete.cases(basket), ]
view(basket)

#remove rows where item = 'NONE'
basket <- basket[!(basket$Item=="NONE"),]

#Make Item categorical
basket %>% mutate(Item = as.factor(Item))

#make the column numeric
transactionNum <- as.numeric(as.character(basket$Transaction))
#Add new column made above to basket
cbind(basket,transactionNum)

glimpse(basket)

library(plyr)

#Remove unnecessary columns
data <- ddply(basket, c("transactionNum"),
                  function(df1)paste(df1$Item,
                                     collapse = ","))
data

data$transactionNum <- NULL

data

write.csv(data,"sale_data.csv", quote = FALSE, row.names = FALSE)

#store csv into dataframe
data_df = read_csv("sale_data.csv", col_names = FALSE )
glimpse(data_df)

sd <- read.transactions("sale_data.csv", format = 'basket', sep = ',')

sd

summary(sd)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(sd,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#Find Unique Items:
count(basket$Item)

itemsHere <- unique(basket$Item)

view(itemsHere)

count(itemsHere)

#Association Rules

association.rules <- apriori(sd, parameter = list(supp=0.001, conf=0.8,minlen=2))
summary(association.rules)
#Top-most rules w.r.t confidence
rules <- sort(association.rules, decreasing=TRUE,by="conf")
inspect(rules[1:5])


