###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Black Oyster Catchers Abundance data: 
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
# 2014 data loading 
URL_OysC <- "https://workspace.aoos.org/published/file/dfa87109-392b-4da4-b083-42f96e27a2ea/NearshoreBenthicSystemsInGOA_SOP07_BLOY_2014NestDensity_Data_20150105.csv"
OysCGet <- GET(URL_OysC)
OysC1 <- content(OysCGet, as='text')
OysC <- read.csv(file=textConnection(OysC1), stringsAsFactors=F)
head(OysC)

# pre 2014 data loading
BLOYzipd <- tempfile()
download.file("https://workspace.aoos.org/published/file/5221340ce4b0f364fbbb226e/Oystercatcher_Package.zip", BLOYzipd, mode="wb")
BLOYzip1 <- unzip(BLOYzipd, list=TRUE)  # provides a list of all files in zipped file
BLOYzip <- BLOYzip1[grep(".csv", BLOYzip1$Name),]$Name # subsets all the .csv files
BLOYzip_nest <- BLOYzip[grep("nest", BLOYzip)]   # subsets the nest data files

# unzip_read <- function(file_list){
#               # for every .csv file in zipped file list, unzip it & read it, bind all together
#               z <- unzip(BLOYzipd, file_list)
#               rbind.fill(lapply(z, read.csv))  
#               }

#' Unzip and Read Files Function
#' @param file_list 
#' @return
#' @export
#' @examples
unzip_read <- function(file_list){
              # for every .csv file in zipped file list, unzip it & read it, bind all together
              zz <- lapply(file_list, FUN = function(x) read.csv(x, stringsAsFactors = FALSE))
              # Bryce added this when the above wasn't working for an intern...saving here just in case...
              #  zz <- lapply(file_list, function(x) read.csv(unz(BLOYzipd, x), stringsAsFactors = FALSE))
              rbind.fill(zz)  
              }

BLOY_nest1 <- unzip_read(BLOYzip_nest)
unlink(BLOYzipd)

# Cleaning, filtering, etc. 
# clean first data frame (pre 2014 data)
BLOY_nest1$NORTH[is.na(BLOY_nest1$NORTH)] <- as.numeric(BLOY_nest1$NORTH_[is.na(BLOY_nest1$NORTH)])
BLOY_nest1[BLOY_nest1 == "."] <- NA  # replace "." with NA in the entire data frame

BLOY_nest <- BLOY_nest1 %>% 
             select(-X,-X.1,-X.2,-Notes,-NORTH_,-START.TIME,-END.TIME) %>%
             dplyr::rename(BLOCK.NUMBER=BLOCK, Region=REGION, Nest_Site=NEST_SITE.., 
                           Adults_Num=X._ADULTS, Eggs_Num=X._EGGS, Chicks_Num=X._CHICKS, 
                           Prey_Collected=PREY_COLL., LAT=NORTH, LON=WEST, Year=YEAR) %>%
             mutate(Region = replace(as.character(Region), Region=="PWS", "WPWS"),  
                    Site_Name = ifelse((SITE=="RI1" & Region=="KATM"),'Kukak Bay',
                                ifelse((SITE=="RI1" & Region=="KEFJ"),'Aialik Bay',       
                                ifelse((SITE=="RI1" & Region=="WPWS"),'Hogan Bay',   
                                ifelse((SITE=="RI2" & Region=="KATM"),'Kaflia Bay',
                                ifelse((SITE=="RI2" & Region=="KEFJ"),'McCarty Fjord',
                                ifelse((SITE=="RI2" & Region=="WPWS"),'Iktua Bay',  
                                ifelse((SITE=="RI3" & Region=="KATM"),'Kinak Bay',  
                                ifelse((SITE=="RI3" & Region=="KEFJ"),'Nuka Bay',  
                                ifelse((SITE=="RI3" & Region=="WPWS"),'Whale Bay',
                                ifelse((SITE=="RI4" & Region=="KATM"),'Amalik Bay',  
                                ifelse((SITE=="RI4" & Region=="KEFJ"),'Nuka Passage',       
                                ifelse((SITE=="RI4" & Region=="WPWS"),'Johnson Bay',    
                                ifelse((SITE=="RI5" & Region=="KATM"),'Takli Island',  
                                ifelse((SITE=="RI5" & Region=="KEFJ"),'Harris Bay', 
                                ifelse((SITE=="RI5" & Region=="WPWS"),'Herring Bay',   
                                ""))))))))))))))),  
                    Eggs_Num = replace(Eggs_Num, Eggs_Num %in% c("U","N/A"), NA),
                    Chicks_Num = replace(Chicks_Num, Chicks_Num %in% c("U","N/A"), NA)
                    )

# clean second data frame (2014)
OysC[OysC == "."] <- NA  # replace "." with NA in the entire data frame

OysC2 <- OysC %>%
         select(-X,-Notes) %>%  # remove weird blank column
         dplyr::rename(Nest_Site=NEST_SITE.., Adults_Num=X._ADULTS, Eggs_Num=X._EGGS, 
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
                Year = ifelse(Year %in% c("14-6","2014 - 6"), '2014', Year),  # replace weird first date
                Year = as.numeric(Year),
                Adults_Num = replace(Adults_Num, Adults_Num=="U", NA),
                Eggs_Num = replace(Eggs_Num, Eggs_Num=="U", NA),
                Chicks_Num = replace(Chicks_Num, Chicks_Num=="U", NA)
                )
         
  
  
# bind the two data frames together

OysC3 <- bind_rows(OysC2, BLOY_nest)


OyC_GOA <- OysC3 %>%
           #select(-START.TIME,-END.TIME) %>%
           filter(STATUS %in% c('A','O'), Site_Name!="") %>%
           mutate_each(funs(as.numeric), Adults_Num,Chicks_Num,Eggs_Num) %>% # change columns to numeric
           group_by(Region, Site_Name, Year) %>%
           summarise(BLOYAdult_breed_n=sum(Adults_Num, na.rm = TRUE), 
                     BLOYEggs_n=sum(Eggs_Num, na.rm = TRUE), 
                     BLOYChicks_live_n=sum(Chicks_Num, na.rm = TRUE)) %>%
           ungroup() %>%
           arrange(Region, Site_Name, Year) %>% 
           select(Region, Site_Name, Year, BLOYAdult_breed_n) # select just breeding Adults

#head(OyC_GOA)








# Status column definitions:
#  A = Active; pair observed with nest, eggs, or chicks
#  F = Failed; broken shells, dead chicks, etc.
#  IA = Inactive; no evidence of a failed nest
#  O = Occupied; could not check nest but pair observed acting broody
#  . = No data collected; none collected
#  NB = Non-breeder
#  U = Unknown
        
      
      
        
