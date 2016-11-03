###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Hobo Logger Temperature Data: pre-processed by Dan Monson in SASS
###########################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############

URL_HWT <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-ueXVIUE02cS1HeFE"
HWTGet <- GET(URL_HWT)
HWT1 <- content(HWTGet, as='text')
HWT <- read.table(file=textConnection(HWT1), sep="\t", header=TRUE)
head(HWT)


