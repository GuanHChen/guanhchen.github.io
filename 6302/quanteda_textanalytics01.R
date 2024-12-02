# Sample program for using quanteda for text modeling and analysis
# Documentation: vignette("quickstart", package = "quanteda")
# Website: https://quanteda.io/

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readr)
library(ggplot2)

# Twitter data about President Biden and Xi summit in Novemeber 2021
# Do some background search/study on the event
# 
summit <- read_csv("https://raw.githubusercontent.com/datageneration/datamethods/master/textanalytics/summit_11162021.csv")
print(summit)

sum_twt = summit$text
toks = tokens(sum_twt)
sumtwtdfm <- dfm(toks)
class(toks)

# Latent Semantic Analysis 
## (https://quanteda.io/reference/textmodel_lsa.html)

sum_lsa <- textmodel_lsa(sumtwtdfm, nd=4,  margin = c("both", "documents", "features"))
summary(sum_lsa)
head(sum_lsa$docs)
class(sum_lsa)

tweet_dfm <- tokens(sum_twt, remove_punct = TRUE) %>%
  dfm()
head(tweet_dfm)

tag_dfm <- dfm_select(tweet_dfm, pattern = "#*")
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag, 10)
library("quanteda.textplots")
tag_fcm <- fcm(tag_dfm)
head(tag_fcm)
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 50, edge_alpha = 0.8, edge_size = 1)
user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
topuser <- names(topfeatures(user_dfm, 50))
head(topuser, 20)
user_fcm <- fcm(user_dfm)
head(user_fcm, 20)

user_fcm <- fcm_select(user_fcm, pattern = topuser)
textplot_network(user_fcm, min_freq = 20, edge_color = "firebrick", edge_alpha = 0.8, edge_size = 1)



