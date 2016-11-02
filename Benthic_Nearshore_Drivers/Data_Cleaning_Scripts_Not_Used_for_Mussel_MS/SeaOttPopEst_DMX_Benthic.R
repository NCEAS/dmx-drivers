###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Sea Otter Population Estimate data: 
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
#############

URL_SOp <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uNHRnWUxYaVEyTFk"
SOpGet <- GET(URL_SOp)
SOp1 <- content(SOpGet, as='text')
SOp <- read.csv(file=textConnection(SOp1),strip.white=TRUE)
head(SOp)


SOPop <- SOp %>%
         select(-X,-X.1,-X.2,-X.3) %>%
         # remove asterisk after Katmai
         mutate(Region = gsub("\\*","",Region)) %>%
         dplyr::rename(Region_Long=Region, Otter_Abun_est=Abun_estimate) %>%
         filter(!Region_Long %in% c("Katmai1"),
                !is.na(Year)) %>%
         mutate(Region = ifelse((Region_Long %in% c("Katmai")), "KATM",
                         ifelse((Region_Long %in% c("Kenai Fjords")), "KEFJ",
                         ifelse((Region_Long %in% c("Western PWS")), "WPWS","")))) %>%
         select(Region, Year, Otter_Abun_est, Otter_density_per_km2)




