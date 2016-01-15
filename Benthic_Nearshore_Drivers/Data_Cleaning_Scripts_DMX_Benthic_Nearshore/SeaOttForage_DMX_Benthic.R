###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Sea Otter foraging data: 
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
library(readxl)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############
# Sea otter diet (Proportion of biomass provided by clams, mussels, crabs, and other)

# pre 2002-2012 data
URL_SOf <- "https://workspace.aoos.org/files/302004/FINAL_2002-2012_all_forage_prepped_for_forage_DB_15july2015.xlsx"

#Test 1
testSOf1 <- read_excel(URL_SOf)

#Test 2
SOfGet <- GET(URL_SOf)
testSOf2 <- read_excel(SOfGet)

head(testSOf)


#URL_SOf <- "https://workspace.aoos.org/published/file/52211fd1e4b067e4402e7ca8/GWA_Benthic_Sea_Otter_Foraging_Data_2006-2012_28Aug2013.csv"
#SOfGet <- GET(URL_SOf)
#SOf1 <- content(SOfGet, as='text')
#SOf <- read.csv(file=textConnection(SOf1))


# 2014 data
URL_SOf2 <- "https://workspace.aoos.org/published/file/e6bf00c8-1914-41c5-a5fe-4047ddad2bfb/NearshoreBenthicSystemsInGOA_SOP02_SeaOtterForageObservations2014_Data_29May2015.csv"
SOf2Get <- GET(URL_SOf2)
SOf2 <- content(SOf2Get, as='text')
SOf2 <- read.csv(file=textConnection(SOf2))
head(SOf2)

# clean first data frame (pre 2014 data)
SOf[SOf == "."] <- NA  # replace "." with NA in the entire data frame

SOf_A <- SOf %>%
         rename(Year=Period, Region=REGION, bout_id=BOUT, bout_date=DATE ) %>%
         filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%
         select(-Area,-Season) %>%          
         mutate_each(funs(as.character), Year) %>% # change class of column   
         mutate(Site_Name = ifelse((SITE %in% c("KUKAK B-10-01I","KUKAK/DEVILS COVE-10-RI1","Kukak/Yugnat-10-RI1",
                                                "Kukak/Devil's Cove-10-RI1","Kukak; Devils cove","Devils Cove",
                                                "Kukak","Devil's Cove","DEVIL'S COVE") & Region=="KATM"),'Kukak Bay',
                            ifelse((SITE %in% c("TAKLI/MINK-10-RI4/5","TAKLI/LITTLE MINK-10-RI5/RI4",
                                                "TAKLI/MINK Is.-10-RI5","Takli/Ilktugitak-10-RI5","Takli/Mink-10-RI5",
                                                "Takli-10-RI5","Takli Island","MINK IS.-10-05I","Little Mink","Little MInk",
                                                "MINK ISLAND","ILKTUGIDAK") & Region=="KATM"),'Takli Island',
                            ifelse((SITE %in% c("Amalik Bay-10-RI4","Amalik; .","Amalik; Ilktigadak","Amalik; Mink",
                                                "Amalik; Little Mink","AMALIK/MINK ISL-10-RI5/RI4"
                                                ) & Region=="KATM"),'Amalik Bay',
                            ifelse((SITE %in% c("KAFLIA/CAPE GULL-10-RI2") & Region=="KATM"),'Kaflia Bay',
                            ifelse((SITE %in% c("Aialik-5-RI1","Aialik/Squab Isl-5-RI1","Pederson/Aialik",
                                                "Pederson/ Aialik","Pedersen","Aialik","AIALIK","Pederson-5-RI1"
                                                ) & Region=="KEFJ"),'Aialik Bay',
                            ifelse((SITE %in% c("MCCARTY LAGOON-5-RI2","McCarty-5-RI2","McCarty","McCarty Lagoon",
                                                "McCarty; James Lagoon","entrance mccarthy","James Lagoon; Outside",
                                                "McCarty-outside beach","JAMES LAGOON","MCCARTY LAGOON",
                                                "James Lagoon","Outside James Lagoon"
                                                ) & Region=="KEFJ"),'McCarty Fjord',
                            ifelse((SITE %in% c("Harris; near mussel site","Harris","Sea Otter Cove-5-RI5",
                                                "Otter cove-5-RI5","OTTER COVE","Otter  Cove","Otter Cove",
                                                "Outside Otter Cove") & Region=="KEFJ"),'Harris Bay',   
                            ifelse((SITE %in% c("TONSINA-5-RI4","Nuka-5-RI4","Tonsina-5-RI4","inside Tonsina",
                                                "Tonsina","Tonsina; tip of L Island","Tonsina; L Island","TONSINA"
                                                ) & Region=="KEFJ"),'Nuka Passage',
                            ifelse((SITE %in% c("NUKA BAY","BEAUTIFUL-5-RI3","Surprise Bay-5-RI3/RI4",
                                                "Beautiful Island","beauty bay") & Region=="KEFJ"),'Nuka Bay',
                            ifelse((SITE %in% c("IKTUA","Iktua Bay","Iktua Passage","IKTUA BAY"
                                                ) & Region=="PWS"),'Iktua Bay',
                            ifelse((SITE %in% c("JOHNSON","Johnson Bay","MOUTH OF JOHNSON BAY","JOHNSON BAY",
                                                "JOHNSON BAY; KNIGHT ISL","N. SQUIRREL BAY","SQUIRREL","S. SQUIRREL"
                                                ) & Region=="PWS"),'Johnson Bay',  
                            ifelse((SITE %in% c("HERRING BAY","Herring Bay") & Region=="PWS"),'Herring Bay',
                            ifelse((SITE %in% c("Whale Bay","ELESHANSKY COVE; WHALE BAY","WHALE BAY"
                                                ) & Region=="PWS"),'Whale Bay',
                            ifelse((SITE %in% c("OLSON BAY") & Region=="PWS"),'Olsen Bay',
                            ifelse((SITE %in% c("SIMPSON BAY","MOUTH OF SIMPSON BAY") & Region=="PWS"),'Simpson Bay',       
                            ifelse((SITE %in% c("GALENA BAY") & Region=="PWS"),'Galena Bay',
                            ifelse((SITE %in% c("BOI","BAY OF ISLES; KIM'S ROCK","Bay of Isles",
                                                "Bay of Isles S. Bite","BAY OF ISLES",
                                                "BOI; KIM'S ROCK","Eagles Nest Isl/BOI"
                                                ) & Region=="PWS"),'Herring Bay',
                            ifelse((FOCAL.OTTER.COORD.NORTH.DD %in% c("60.10388","60.11315","60.112",
                                                                      "60.10924","60.11121","60.11513"
                                                                      ) & Region=="PWS"),'Iktua Bay',  
                            ifelse((FOCAL.OTTER.COORD.NORTH.DD %in% c("60.92948","60.92477"
                                                                      ) & Region=="PWS"),'Galena Bay',  
                            ifelse((FOCAL.OTTER.COORD.NORTH.DD %in% c("60.33932","60.34066"
                                                                      ) & Region=="PWS"),'Johnson Bay',   
                                  'OTHER'))))))))))))))))))))
                )
                
# PWS Mystery Sites: 
#     NASTY BITE                 FOUL PASS                  PASS IS                    PUKUK              
#     NASTY BIGHT                LOWER PASS                 AGULIAK                    POWP                
#     Lower Pass                 Danger Island              Ismailov Island            GUGNAK BAY                   
#     Hogg Bay                   Foul Pass                  S. Latouche                Latouche Passage          
#     Ismaylov Is.               Danger Is                  Gibbon Anchorage           STICKDALE HARBOR 
#     SW Latouche                Fox Farm                   South Bight Overlook       
#     S. BIGHT                   N. ISLAND                  NORTH ISLAND              
#     GRAVINA                    DEEP BAY                   DEEP BAY; NEAR CORDOVA                                            
#     GREEN ISLAND; GIBBON ANCH  Green Island               BARRIER ISL; GREEN ISLAND   
#     GREEN ISLAND GIBBON ANCH   Green Is                   LITTLE GREEN               GREEN ISLAND  
# KEFJ Mystery Sites:    near BLOY x2-08      Near BLOY x2-08                 
# KATM Mystery Sites:   Ugyak; Ugyak                                              
                
                
#write.csv(SOf_A, file = "SeaOttForage_older2.csv", row.names=FALSE)
                
                

# clean 2014 data
SOf_B <- SOf2 %>%
         rename(Region=area_cd, SITE=specific_location_txt) %>% 
         select(-region_cd) %>%
         mutate(Year = sapply(strsplit(as.character(bout_date), split="/") , function(x) x[3]),
                Site_Name = ifelse((SITE %in% c("West End Nuka Pass","Nuka Pass","North Nuka Pass","Yalik Bay",
                                                "Yalik Beach"
                                                ) & Region=="KEFJ"),'Nuka Passage',
                            ifelse((SITE %in% c("Nuka Island","Nuka","Beautiful Island","islands west of Nuka",
                                                "West Nuka","Islands west of Nuka Island","islands west of Nuka",
                                                "Nuka Island West") & Region=="KEFJ"),'Nuka Bay',
                            ifelse((SITE %in% c("Harris Bay","Harris Bay Surf Beach","Harris Bay Spit"
                                                ) & Region=="KEFJ"),'Harris Bay',
                            ifelse((SITE %in% c("James Lagoon") & Region=="KEFJ"),'McCarty Fjord',       
                            ifelse((site_name %in% c("Nuka Passage, KEP_B05_RI_04") & Region=="KEFJ"),'Nuka Passage',
                            ifelse((site_name %in% c("Nuka Bay, KEP_B05_RI_03") & Region=="KEFJ"),'Nuka Bay', 
                            ifelse((site_name %in% c("Harris Bay, KEP_B05_RI_05") & Region=="KEFJ"),'Harris Bay',       
                            ifelse((site_name %in% c("Johnson Bay, PWS_B08_RI_04") & Region=="WPWS"),'Johnson Bay', 
                            ifelse((site_name %in% c("Herring Bay, PWS_B08_RI_05") & Region=="WPWS"),'Herring Bay',
                            ifelse((site_name %in% c("Iktua Bay, PWS_B08_RI_02") & Region=="WPWS"),'Iktua Bay',
                            ifelse((site_name %in% c("Takli Island, AKP_B10_RI_05") & Region=="KATM"),'Takli Island',
                            ifelse((site_name %in% c("Amalik Bay, AKP_B10_RI_04") & Region=="KATM"),'Amalik Bay',       
                            ifelse((site_name %in% c("Kukak Bay, AKP_B10_RI_01") & Region=="KATM"),'Kukak Bay',    
                            ifelse((site_name %in% c("McCarty Fjord, KEP_B05_RI_02") & Region=="KEFJ"),'McCarty Fjord',
                                   'OTHER'))))))))))))))
                ) %>% 
         select(-site_name)
             
                
# bind the dataframes together

SOF <- bind_rows(SOf_A, SOf_B)  ### THIS HAS MAJOR ISSUES!!!!!!!!!!!!!!!!!!!!



