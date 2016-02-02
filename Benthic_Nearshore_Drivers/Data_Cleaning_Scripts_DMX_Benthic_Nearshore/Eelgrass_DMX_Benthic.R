###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Eelgrass Percent Cover: 
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
URL_ElG <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uY2s4ZUFoZ05IMEU"
ElGGet <- GET(URL_ElG)
ElG1 <- content(ElGGet, as='text')
ElG <- read.csv(file=textConnection(ElG1))
ElG <- rename(ElG, Year=Sample_Year)
#head(ElG)

