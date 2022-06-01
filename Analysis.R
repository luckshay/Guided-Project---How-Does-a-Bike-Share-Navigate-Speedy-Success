library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

setwd("E:/Capstone Project/Case Study 1 (Guided)") #sets your working directory to simplify calls to data ...
getwd()

#import data

may <- read.csv("may_21.csv")
jun <- read.csv("jun_21.csv")
jul <- read.csv("jul_21.csv")
aug <- read.csv("aug_21.csv")
sep <- read.csv("sep_21.csv")
oct <- read.csv("oct_21.csv")
jan <- read.csv("jan_22.csv")
feb <- read.csv("feb_22.csv")
mar <- read.csv("mar_22.csv")
apr <- read.csv("apr_22.csv")

#colnames(jan)

