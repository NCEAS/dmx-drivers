###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Sea Otter Energy Recovery data: 
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
# NOTE: Sea Otter Energy Recovery Rates calculated by Dan Monson at USGS in Anchorage
# Units are "Rate of Energy Gain kcal/minute/time period"
#############

URL_SO <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uSHRPZ0pZNGFJTDA"
SOGet <- GET(URL_SO)
SO1 <- content(SOGet, as='text')
SOer <- read.csv(file=textConnection(SO1),skip=1,strip.white=TRUE)
head(SOer)


REG <- SOer %>%
       select(-X.1,-X.2,-X.3,-X.4) %>%
       dplyr::rename(Comments=X) %>%
       mutate(Year = str_sub(SOer$Period, -4,-1),
              Region = str_sub(SOer$Period, 1,4),
              Season = str_sub(SOer$Period, 5,-5)) %>%
       select(Region,Year,Season,Long.Term) %>%
       group_by(Region,Year) %>%
       summarise(SOtt_AnnMnEngRec = mean(Long.Term)) %>%
       ungroup()






