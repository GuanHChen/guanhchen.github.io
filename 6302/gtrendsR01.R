# EPPS 6302: Google Trends data 
# Sample program for using gtrendsR for collecting Google Trends data
# Documentation: vignette("quickstart", package = "gtrendsR")
# Website: https://cran.r-project.org/web/packages/gtrendsR/index.html
# GitHub: https://github.com/PMassicotte/gtrendsR


## Install package
install.packages("gtrendsR")

## Load library and run gtrends
library(gtrendsR)
HarrisTrumpElection = gtrends(c("Trump","Harris","election"), time = "all")

## Select data for plotting
HarrisTrumpElection_interest <- HarrisTrumpElection$interest_over_time

## Plot data
par(family="Georgia")
plot(HarrisTrumpElection_interest$hits, type="l")

## Try another dataset?
borderimg = gtrends("border immigrant", time = "all")

## Collect data by timeframe

gtrends(c("Harris", "Trump"), time = "now 1-H") # last hour
gtrends(c("Harris", "Trump"), time = "today 1-m") # last 30 days

## Collect data by country

tg_gb <- gtrends(c("immigrants"), geo = c("GB", "US"), time = "all") 

## Check country codes
 
data("countries")

