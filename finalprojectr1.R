install.packages("rvest")
install.packages("NLP")
install.packages("Rstem")
install.packages("quanteda")

setwd("E:/ENGINEERING MANAGEMENT/DATA MINING AND MACHINE LEARNING/FINAL PROJECT")
#Getting text
library(rvest)
library(curl)
library(magrittr)
library()
url <- "https://www.amazon.com/Amazon-Kindle-Paperwhite-6-Inch-4GB-eReader/product-reviews/B00OQVZDJM/ref=cm_cr_arp_d_viewopt_sr?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=recent&pageNumber=1&filterByStar=three_star"
data <- read_html(paste(url,1,sep = ""))
review <- data %>%
  html_nodes(".review-text") %>%
  html_text()

for(level in c(2:173)){
  data <- read_html(paste(url,level,sep = ""))
  review <- c(review,data %>%
                html_nodes(".review-text") %>%
                html_text())
}
review <- as.data.frame(review)

#Sentiment Analisis
library(NLP)
library(Rstem)
require(quanteda)

positive <- read.csv(file = "positive_words.txt", header = F, stringsAsFactors=FALSE)
negative <- read.csv(file = "negative_words.txt", header = F, stringsAsFactors=FALSE)
stem <- function(list){
  for(i in c(1:nrow(list))){
    list[i,1] <- wordStem(String(list[i,1]))
  }
  list <- unique(list[,1] )
}

positive <- stem(positive)
negative <- stem(negative)
positiveCount<- 0
negativeCount<- 0
pos <- 0
neg <- 0
neut <- 0

for(level in c(1:nrow(review))){
  str <- String(review$review[level])
  tokens <- str[wordpunct_tokenizer(str)]
  stem <- wordStem(tokens)
  positiveCount <- positiveCount + length(intersect(stem,positive))
  negativeCount <- negativeCount + length(intersect(stem,negative))
  review$PositiveWordsCount[level] <- positiveCount
  review$NegativeWordsCount[level] <- negativeCount
  diffr = positiveCount-negativeCount
  
  if( diffr > 0) {
    pos = pos + 1
  } else {
    neg = neg + 1
  }
}
library(ggplot2)
ggplot(positiveCount,negativeCount)