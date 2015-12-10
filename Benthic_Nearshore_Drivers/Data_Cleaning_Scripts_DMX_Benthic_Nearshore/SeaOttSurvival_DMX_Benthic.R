###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Sea Otter survival/carcass data: 
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
# Sea Otter Survival data

# 2006 - 2012
URL_SOs <- "https://workspace.aoos.org/published/file/522139b0e4b067e4402e7cb4/GWA_Benthic_Sea_Otter_Carcass_2006to2012_30Aug2013.csv"
SOsGet <- GET(URL_SOs)
SOs1 <- content(SOsGet, as='text')
SOs <- read.csv(file=textConnection(SOs1),strip.white=TRUE)
head(SOs)

# Cleaning the data
SOs[SOs == "."] <- NA  # replace "." with NA in the entire data frame

SOS <- SOs %>%
       rename(Year=YEAR) %>%
       filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%
       mutate(Region = ifelse((AREA=="KATM"),'KATM',
                       ifelse((AREA=="KEFJ"),'KEFJ',
                       ifelse((AREA=="PWS" & SITE %in% c("Cedar Bay","Esther Passage","Bettles Bay",
                                                         "Perry Island","Unakwik Inlet")),'NPWS',
                       ifelse((AREA=="PWS" & SITE %in% c("Galena Bay","Port Fidalgo","Olsen Bay",
                                                         "Simpson Bay","Observation Island")),'EPWS',        
                       ifelse((AREA=="PWS" & SITE %in% c("Hogan Bay","Hogan B","Iktua Bay","Whale Bay",
                                                         "Johnson Bay","Herring Bay","Northwest Bay",
                                                         "Herring Bay- Bear Cove","Disk Island","Green Is",
                                                         "Green Is/Barrier","Channel Is","Little Green",
                                                         "Latouche Is","Danger Is","Fox Farm","Evans Is",
                                                         "South Arm Bay of Isles","Eleanor Is","Lower Pass",
                                                         "Lower Pass; nasty bight access beach",
                                                         "Naked Island","Knight Is","Bay of Isles",
                                                         "Ingot Island")),'WPWS',
                       ifelse((LAT_DD=="60.24045" & LON_DD=="147.7567"),'WPWS',
                       ifelse((LAT_DD=="60.52808" & LON_DD=="147.0079"),'WPWS',
                       ifelse((LAT_DD=="60.53012" & LON_DD=="147.6088"),'WPWS',
                       "unknown"))))))))
              ) %>%
       filter(!Region=="unknown") %>%
       
  

#  mutate(Species_Name = revalue(Species_Name, c("Nucella sp."="Nucella sp")))
#  Eggs_Num = replace(Eggs_Num, Eggs_Num %in% c("U","N/A"), NA),
  


head(SOS)




#  x <- readClipboard()



