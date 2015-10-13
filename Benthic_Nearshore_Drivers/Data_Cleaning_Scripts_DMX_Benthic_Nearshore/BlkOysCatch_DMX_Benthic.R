###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Black Oyster Catchers: 
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
# Oyster Catchers
URL_OysC <- "https://workspace.aoos.org/published/file/dfa87109-392b-4da4-b083-42f96e27a2ea/NearshoreBenthicSystemsInGOA_SOP07_BLOY_2014NestDensity_Data_20150105.csv"
OysCGet <- GET(URL_OysC)
OysC1 <- content(OysCGet, as='text')
OysC <- read.csv(file=textConnection(OysC1), stringsAsFactors=F)
head(OysC)

# Cleaning, filtering, etc. 
OysC[OysC == "."] <- NA  # replace "." with NA in the entire data frame

OyC_GOA <- OysC %>%
           select(-X) %>%  # remove weird blank column
           rename(Nest_Site=NEST_SITE.., Adults_Num=X._ADULTS, Eggs_Num=X._EGGS, 
                  Chicks_Num=X._CHICKS, Prey_Collected=PREY_COLL., Region=Block.Name) %>%
           mutate(Site_Name = ifelse((SITE=="RI-01" & Region=="KATM"),'Kukak Bay',  # add Site names
                              ifelse((SITE=="RI-02" & Region=="KATM"),'Kaflia Bay',
                              ifelse((SITE=="RI-03" & Region=="KATM"),'Kinak Bay',
                              ifelse((SITE=="RI-04" & Region=="KATM"),'Amalik Bay',
                              ifelse((SITE=="RI-05" & Region=="KATM"),'Takli Island',
                              ifelse((SITE=="RI-01" & Region=="KEFJ"),'Aialik Bay',
                              ifelse((SITE=="RI-02" & Region=="KEFJ"),'McCarty Fjord',
                              ifelse((SITE=="RI-03" & Region=="KEFJ"),'Nuka Bay',
                              ifelse((SITE=="RI-04" & Region=="KEFJ"),'Nuka Passage',
                              ifelse((SITE=="RI-05" & Region=="KEFJ"),'Harris Bay',       
                              ifelse((SITE=="RI-01" & Region=="WPWS"),'Hogan Bay',
                              ifelse((SITE=="RI-02" & Region=="WPWS"),'Iktua Bay',
                              ifelse((SITE=="RI-03" & Region=="WPWS"),'Whale Bay',
                              ifelse((SITE=="RI-04" & Region=="WPWS"),'Johnson Bay',
                              ifelse((SITE=="RI-05" & Region=="WPWS"),'Herring Bay',        
                              ""))))))))))))))),
                  Year = sapply(strsplit(as.character(DATE), split="/") , function(x) x[3]), # create Sample Year column
                  Year = ifelse(Year == "14-6", '2014', Year),  # replace weird first date 
                  Adults_Num = replace(Adults_Num, Adults_Num=="U", NA),
                  Eggs_Num = replace(Eggs_Num, Eggs_Num=="U", NA),
                  Chicks_Num = replace(Chicks_Num, Chicks_Num=="U", NA)
                  ) %>%
           filter(STATUS %in% c('A','O'), Site_Name!="") %>%
           mutate_each(funs(as.numeric), Adults_Num,Chicks_Num,Eggs_Num) %>% # change columns to numeric
           group_by(Region, Site_Name, Year) %>%
           summarise(BLOYAdult_breed_n=sum(Adults_Num, na.rm = TRUE), 
                     BLOYEggs_n=sum(Eggs_Num, na.rm = TRUE), 
                     BLOYChicks_live_n=sum(Chicks_Num, na.rm = TRUE)) %>%
           ungroup() %>%
           arrange(Region, Site_Name, Year) %>% 
           select(Region, Site_Name, Year, BLOYAdult_breed_n) # select just breeding Adults
head(OyC_GOA)


# Status column definitions:
#  A = Active; pair observed with nest, eggs, or chicks
#  F = Failed; broken shells, dead chicks, etc.
#  IA = Inactive; no evidence of a failed nest
#  O = Occupied; could not check nest but pair observed acting broody
#  . = No data collected; none collected
#  NB = Non-breeder
#  U = Unknown
        
      
      
        
