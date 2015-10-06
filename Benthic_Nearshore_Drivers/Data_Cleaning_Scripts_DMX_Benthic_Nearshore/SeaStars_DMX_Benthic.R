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

# Cleaning, filtering, etc
SS_GOA <- SS %>%
          rename(Year=Sample_Year) %>%
          filter(Year %in% c(2010, 2011, 2012, 2013, 2014, 2015)) %>% # taking out years before 2010
          mutate(SS_n_m2 = Density..individuals_per_100_sq_m./100) %>% # getting n per m2
          # STOP here if you want species-level data
          group_by(Site_Name, Year) %>%
          summarise(SS_sum_n_m2=sum(SS_n_m2)) %>%
          ungroup() %>%
          arrange(Site_Name, Year)
SS_GOA





