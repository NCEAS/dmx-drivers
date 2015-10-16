###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Black Oyster Catcher Diet Data : 
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
# 2014 data
URL_BLOYD <- "https://workspace.aoos.org/published/file/ef68bf7a-f3f2-46e3-8041-a3d1c708903c/NearshoreBenthicSystemsInGOA_SOP07_BLOY_2014ChickDiet_Data_20150105.csv"
BLOYDGet <- GET(URL_BLOYD)
BLOYD1 <- content(BLOYDGet, as='text')
BLOYD <- read.csv(file=textConnection(BLOYD1))
head(BLOYD)

# pre 2014 data
BLOYzipd <- tempfile()
download.file("https://workspace.aoos.org/published/file/5221340ce4b0f364fbbb226e/Oystercatcher_Package.zip", BLOYzipd, mode="wb")
BLOYzip1 <- unzip(BLOYzipd, list=TRUE)

BLOYzip <- BLOYzip1[grep(".csv", BLOYzip1$Name),]$Name # subsets all the .csv files
BLOYzip_diet <- BLOYzip[grep("diet", BLOYzip)]   # subsets the diet data files


unzip_read <- function(file_list){
              # for every .csv file in zipped file list, do the following 
              z <- unzip(BLOYzipd, file_list)
              rbind.fill(lapply(z, read.csv))
              }

BLOY_diet <- unzip_read(BLOYzip_diet)
unlink(BLOYzipd)

# Cleaning, filtering, etc. 
# clean first data frame (pre 2014 data)







# bind the two data frames together
BLOYD2 <- rbind.fill(BLOYD, BLOY_diet)

# more cleaning
BLOYD2[BLOYD2 == "."] <- NA  # replace "." with NA in the entire data frame

BLOYD_GOA <- BLOYD2 %>%
             rename(REGIONBLOCK=Region,Region=Block.Name,Size_mm=Size..mm.) %>%
             mutate(Year = sapply(strsplit(as.character(Date), split="/") , function(x) x[3]), # create Sample Year column
                    Year = ifelse(Year == "14", '2014', Year),  # replace weird first date 
                    Site_Name = ifelse((Site=="Ninagiak" & Region=="KATM"),'Ninagiak Island',
                                ifelse((Site=="RI-02" & Region=="KATM"),'Kaflia Bay',
                                ifelse((Site=="RI-03" & Region=="KATM"),'Kinak Bay',
                                ifelse((Site=="RI-04" & Region=="KATM"),'Amalik Bay',
                                ifelse((Site=="RI-04" & Region=="KEFJ"),'Nuka Passage',
                                ifelse((Site=="RI-05" & Region=="KATM"),'Takli Island',
                                ifelse((Site=="Shakun" & Region=="KATM"),'Shakun',
                                ifelse((Site=="RI-01" & Region=="KEFJ"),'Aialik Bay',
                                ifelse((Site=="2" & Region=="WPWS"),'Iktua Bay',
                                ifelse((Site=="3" & Region=="WPWS"),'Whale Bay',
                                ifelse((Site=="5" & Region=="WPWS"),'Herring Bay', 
                                "")))))))))))       
                    ) %>%
             mutate_each(funs(as.numeric), Size_mm) # change class of column 

BLOYD_Abun <- BLOYD_GOA %>%
              count(Region,Year,Site_Name,Nest.site..,Species) %>%
              rename(BLOYDietItem_Abun=n)
  
BLOYD_Size <- BLOYD_GOA %>%
              group_by(Region,Year,Site_Name,Nest.site..,Species) %>%
              summarize(BLOYDietItem_meanSzmm = mean(Size_mm, na.rm=TRUE)) %>%
              ungroup()

# still need to add calculation of the proportion of biomass provided by 
# limpets, mussels, other                      
                      
                      
                      
                      
                      