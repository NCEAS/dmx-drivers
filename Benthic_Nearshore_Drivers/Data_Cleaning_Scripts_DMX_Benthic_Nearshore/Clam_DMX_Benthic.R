###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Clam abundance data: 
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
# Clam data           # note quadrates 0.25m squared in size

URL_C <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-udGZlTTF6MVJjR1U"
CGet <- GET(URL_C)
C1 <- content(CGet, as='text')
C <- read.csv(file=textConnection(C1))
head(C)

Clam <- C %>%
        rename(Site_Name=site.name, Quadrat=quad.,Size_mm=size..mm.) %>%
        mutate(Year = sapply(strsplit(as.character(date), split="-") , function(x) x[3]),
               Year = paste("20",Year,sep=""),
               Region = ifelse((Site_Name %in% c("Galena Bay","Observation Island",
                                                 "Olsen Bay","Port Fidalgo",
                                                 "Simpson Bay")),'EPWS',
                        ifelse((Site_Name %in% c("Amalik Bay","Kaflia Bay","Kinak Bay",
                                                 "Kukak Bay","Ninagiak Island",
                                                 "Takli Island")),'KATM',
                        ifelse((Site_Name %in% c("Aialik Bay","Harris Bay","McCarty Fjord",
                                                 "Nuka Bay","Nuka Passage")),'KEFJ',
                        ifelse((Site_Name %in% c("Chinitna Bay","Chisik Island","Johnson Creek",
                                                 "Polly Creek","Tukendni Bay")),'LACL',
                        ifelse((Site_Name %in% c("Bettles Bay","Cedar Bay","Esther Passage",
                                                 "Perry Island","Unakwik Inlet")),'NPWS',       
                        ifelse((Site_Name %in% c("Disk Island","Herring Bay",
                                                 "Herring Bay-Bear Cove","Herring Bay-Southwest",
                                                 "Hogan Bay","Iktua Bay","Johnson Bay",
                                                 "Northwest Bay","Whale Bay")),'WPWS',""))))))) %>%
        filter(Year %in% c(2010,2011,2012,2013,2014,2015))
        
BiV_Abun_Size <- function(df, abun_column_name, size_column_name) { 
                 # calculate the abundance (count number of rows)
                 A <- df %>%
                      count(Region, Site_Name, Year, Quadrat) %>%
                      mutate_(.dots= setNames(list(~n*4), abun_column_name)) %>%
                      select_("Region", "Site_Name", "Year", "Quadrat", abun_column_name)
                 # calculate the mean size (average per quadrat)
                 B <- df %>%
                      filter(!is.na(Size_mm)) %>%
                      group_by(Region,Site_Name,Year,Quadrat) %>% 
       #               summarise(size_column_name=mean(Size_mm)) %>%
                      summarise_(.dots = setNames(list(~mean(Size_mm)), size_column_name)) %>%  # mean of elevations together
                      ungroup() %>%
                      select_("Region", "Site_Name", "Year", "Quadrat", size_column_name)
                 # bind the two data frames together
                 merge(A, B, by=c("Region","Site_Name","Year","Quadrat"))
                 }


# Leukoma staminea 
Leuk <- filter(Clam, spp.name=="Leukoma staminea")     
Leuk_Abun_Size <- BiV_Abun_Size(Leuk, "Leukoma_Abun_m2","Leukoma_MnSize_mm")    

# Macoma spp.
Mac <-  filter(Clam, spp.name %in% c("Macoma balthica", "Macoma inquinata","Macoma spp."))          
Maco_Abun_Size <- BiV_Abun_Size(Mac, "Macoma_Abun_m2","Macoma_MnSize_mm")
  
#Saxidomus gigantea
Saxi <- filter(Clam, spp.name=="Saxidomus gigantea")
Saxi_Abun_Size <- BiV_Abun_Size(Saxi, "Saxidomus_Abun_m2","Saxidomus_MnSize_mm")


  




