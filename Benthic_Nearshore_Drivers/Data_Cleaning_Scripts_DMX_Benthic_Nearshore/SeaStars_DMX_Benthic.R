###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Sea Stars (abundance) : 
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
# Sea Stars
URL_SS <-"https://workspace.aoos.org/published/file/5204f963e4b067e4402e6ccd/BenthicNearshoreSystemsInGOA__SeaStars_Data_2006_2012.csv"
SSGet <- GET(URL_SS)
SS1 <- content(SSGet, as='text')
SS <- read.csv(file=textConnection(SS1))
head(SS)

# 2015 data
URL_SS15 <-"https://drive.google.com/uc?export=download&id=0By1iaulIAI-uTFc4Uks3NjgzWVU"
SSGet15 <- GET(URL_SS15)
SS115 <- content(SSGet15, as='text')
SS15 <- read.csv(file=textConnection(SS115))
head(SS15)


# Cleaning
SSm <- SS %>%
       mutate(SS_n_m2 = Density..individuals_per_100_sq_m./100) %>% # getting n per m2
       select(-Density..individuals_per_100_sq_m.)
  
SS15m <- SS15 %>%
         mutate(SS_n_m2 = Density..individuals.per.200.square.m./200) %>% # getting n per m2
         select(-Density..individuals.per.200.square.m.)

SS2 <- bind_rows(SSm, SS15m)

SS_GOA <- SS2 %>%
          rename(Year=Sample_Year) %>%
          filter(Year %in% c(2010, 2011, 2012, 2013, 2014, 2015)) %>% # taking out years before 2010
          # STOP here if you want species-level data
          group_by(Site_Name, Year) %>%
          summarise(SS_sum_n_m2=sum(SS_n_m2)) %>%
          ungroup() %>%
          arrange(Site_Name, Year)
SS_GOA





