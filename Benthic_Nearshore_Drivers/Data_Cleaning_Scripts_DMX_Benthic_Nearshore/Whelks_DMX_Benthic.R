###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Nucella sp. (whelks): 
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
# Nucella 2006 - 2016
URL_NK3 <- "https://drive.google.com/uc?export=download&id=1OVkOw4yFi_iicGQL2ZWcRz0-eD5w3bD5"
NK3Get <- GET(URL_NK3)
WCH33 <- content(NK3Get, as='text')
WCH3 <- read.csv(file=textConnection(WCH33))
head(WCH3)


# Cleaning, filtering
Wlk_GOA <- WCH3 %>%
           mutate(Species_Name = revalue(Species_Name, c("Nucella sp."="Nucella sp"))) %>% # remove "." from Nucella
           filter(Species_Name == "Nucella sp") %>% # extract only the Nucella rows
   #        filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014, 2015))  %>% # taking out years before 2010
           dplyr::rename(Year=Sample_Year, Quadrat=Quadrat_Num)  %>%  # rename columns
           mutate(Whelk_n_m2 = Density..individuals.per.2.square.m./2) %>% # getting n per m2
           group_by(Site_Name, Year, Quadrat) %>%
           summarise(Whelk_Sum_n_m2=sum(Whelk_n_m2)) %>%
           ungroup() %>%
           arrange(Site_Name, Year, Quadrat)

#Wlk_GOA



