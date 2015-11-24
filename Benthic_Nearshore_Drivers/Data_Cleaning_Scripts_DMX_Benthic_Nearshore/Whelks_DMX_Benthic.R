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
URL_NK <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-udnRXQ1hTX3YzMXc"
NKGet <- GET(URL_NK)
WCH1 <- content(NKGet, as='text')
WCH <- read.csv(file=textConnection(WCH1))
head(WCH)

# Cleaning, filtering
Wlk_GOA <- WCH %>%
           mutate(Species_Name = revalue(Species_Name, c("Nucella sp."="Nucella sp"))) %>% # remove "." from Nucella
           filter(Species_Name == "Nucella sp") %>% # extract only the Nucella rows
           filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014, 2015))  %>% # taking out years before 2010
           rename(Year=Sample_Year, Quadrat=Quadrat_Num)  %>%  # rename columns
           mutate(Whelk_n_m2 = Density..individuals.per.2.square.m./2) %>% # getting n per m2
           group_by(Site_Name, Year, Quadrat) %>%
           summarise(Whelk_Sum_n_m2=sum(Whelk_n_m2)) %>%
           ungroup() %>%
           arrange(Site_Name, Year, Quadrat)

Wlk_GOA



