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

BLOY_diet1 <- unzip_read(BLOYzip_diet)  ;  unlink(BLOYzipd)

# Cleaning, filtering, etc. 
# make common genus-level categories
Nucella <- c("Nucella spp.","Nucella lima","Nucella canaliculata","N. lima","N. lamellosa")
Lottia <- c("Lottia","Lottia persona","Lottia digitalis","Lottia scutum","Lottia pelta",
            "L. pelta","L. digitalis","L. scutum")
Mopalia <- c("Mopalia","Mopalia sp.","M. lignosa")
Buccinum <- c("Buccinum baeri","B. baeri")
Mytilus <- c("Mytilus trossulus","M. trossulus")
Hiatella <- c("Hiatella arctica","H. arctica")
Modiolus <- c("Modiolus modiolus","M. modiolus")
Acmea <- c("Acmea mitra","A. mitra")
Katharina <- c("Katharina tunicata","K. tunicata")
Pododesmus <- c("Pododesmus sp.","P. macrochisma")
Tectura <- c("T. scutum","T. persona")                    
Macoma <- c("Macoma sp.")                           
Lirabuccinum <- c("L. dirum")
Saxidomus <- c("S. gigantea")
Leukoma <- c("L. staminea")                           
Telmessus <- c("T. cheiragonus")                
Littorina <- c("L. sitkana")

# N. marginata  ??????????????????????????????                             

# clean first data frame (pre 2014 data)
BLOY_diet1[BLOY_diet1 == "."] <- NA  # replace "." with NA in the entire data frame

BLOY_diet <- BLOY_diet1 %>% 
             rename(Size_mm=Size, Site=Site.) %>% 
             select(-Comment, -Observers) %>%
             filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%
             mutate(Region = replace(as.character(Region), Region=="PWS", "WPWS"),
                    Site_Name = ifelse((Site=="RI1" & Region=="KATM"),'Kukak Bay',
                                ifelse((Site=="RI1" & Region=="KEFJ"),'Aialik Bay',       
                                ifelse((Site=="RI1" & Region=="WPWS"),'Hogan Bay',   
                                ifelse((Site=="RI2" & Region=="KATM"),'Kaflia Bay',
                                ifelse((Site=="RI2" & Region=="KEFJ"),'McCarty Fjord',
                                ifelse((Site=="RI2" & Region=="WPWS"),'Iktua Bay',  
                                ifelse((Site=="RI3" & Region=="KATM"),'Kinak Bay',  
                                ifelse((Site=="RI3" & Region=="KEFJ"),'Nuka Bay',  
                                ifelse((Site=="RI3" & Region=="WPWS"),'Whale Bay',
                                ifelse((Site=="RI4" & Region=="KATM"),'Amalik Bay',  
                                ifelse((Site=="RI4" & Region=="KEFJ"),'Nuka Passage',       
                                ifelse((Site=="RI4" & Region=="WPWS"),'Johnson Bay',    
                                ifelse((Site=="RI5" & Region=="KATM"),'Takli Island',  
                                ifelse((Site=="RI5" & Region=="KEFJ"),'Harris Bay', 
                                ifelse((Site=="RI5" & Region=="WPWS"),'Herring Bay',  
                                ""))))))))))))))),
                    Genus = ifelse((Species %in% Acmea),'Acmea',
                            ifelse((Species %in% Buccinum),'Buccinum',
                            ifelse((Species %in% Hiatella),'Hiatella',
                            ifelse((Species %in% Katharina),'Katharina', 
                            ifelse((Species %in% Leukoma),'Leukoma',  
                            ifelse((Species %in% Lirabuccinum),'Lirabuccinum',   
                            ifelse((Species %in% Littorina),'Littorina',
                            ifelse((Species %in% Lottia),'Lottia',   
                            ifelse((Species %in% Macoma),'Macoma',       
                            ifelse((Species %in% Modiolus),'Modiolus',  
                            ifelse((Species %in% Mopalia),'Mopalia',
                            ifelse((Species %in% Mytilus),'Mytilus',     
                            ifelse((Species %in% Nucella),'Nucella',
                            ifelse((Species %in% Pododesmus),'Pododesmus',    
                            ifelse((Species %in% Saxidomus),'Saxidomus', 
                            ifelse((Species %in% Tectura),'Tectura', 
                            ifelse((Species %in% Telmessus),'Telmessus', 
                            "")))))))))))))))))       
                    ) %>%
             mutate_each(funs(as.numeric), Size_mm) # change class of column           

# clean 2014 data 
BLOYD[BLOYD == "."] <- NA  # replace "." with NA in the entire data frame

BLOYD_D <- BLOYD %>%
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
                              ""))))))))))),
                  Genus = ifelse((Species %in% Acmea),'Acmea',
                            ifelse((Species %in% Buccinum),'Buccinum',
                            ifelse((Species %in% Hiatella),'Hiatella',
                            ifelse((Species %in% Katharina),'Katharina', 
                            ifelse((Species %in% Leukoma),'Leukoma',  
                            ifelse((Species %in% Lirabuccinum),'Lirabuccinum',   
                            ifelse((Species %in% Littorina),'Littorina',
                            ifelse((Species %in% Lottia),'Lottia',   
                            ifelse((Species %in% Macoma),'Macoma',       
                            ifelse((Species %in% Modiolus),'Modiolus',  
                            ifelse((Species %in% Mopalia),'Mopalia',
                            ifelse((Species %in% Mytilus),'Mytilus',     
                            ifelse((Species %in% Nucella),'Nucella',
                            ifelse((Species %in% Pododesmus),'Pododesmus',    
                            ifelse((Species %in% Saxidomus),'Saxidomus', 
                            ifelse((Species %in% Tectura),'Tectura', 
                            ifelse((Species %in% Telmessus),'Telmessus', 
                            "")))))))))))))))))  
                  ) %>%
           mutate_each(funs(as.numeric), Size_mm) # change class of column 


# bind the two data frames together
BLOYD_GOA <- rbind.fill(BLOYD_D, BLOY_diet)


BLOYD_Abun <- BLOYD_GOA %>%
              count(Region,Year,Site_Name,Genus) %>%
              rename(BLOYDietItem_Abun=n)
  
BLOYD_Size <- BLOYD_GOA %>%
              group_by(Region,Year,Site_Name,Genus) %>%
              summarize(BLOYDietItem_meanSzmm = mean(Size_mm, na.rm=TRUE)) %>%
              ungroup()

# still need to add calculation of the proportion of biomass provided by 
# limpets, mussels, other                      
                      
                    
                      
                      
                      