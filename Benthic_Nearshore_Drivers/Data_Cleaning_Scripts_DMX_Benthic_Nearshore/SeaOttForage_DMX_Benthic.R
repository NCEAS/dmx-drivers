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
library(lazyeval)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############
# Sea otter diet (Proportion of biomass provided by clams, mussels, crabs, and other)

# prey biomass conversion from Ben Weitzman based on Oftedal et al. 2007 nutritional analyses
URL_Bm <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uN2FrdzIzLS1FZUk"
BmGet <- GET(URL_Bm)
Bm1 <- content(BmGet, as='text')
BmCalc <- read.csv(file=textConnection(Bm1))
head(BmCalc)

# pre 2002-2012 data
#URL_SOf <- "https://workspace.aoos.org/files/302004/FINAL_2002-2012_all_forage_prepped_for_forage_DB_15july2015.xlsx"
SOf <- read_excel("C://Users//rblake//Documents//NCEAS//GoA Dynamics WG//GW_Nearshore Intertidal Data//Sea_Otter_Stuff//FINAL_2002-2012_all_forage_prepped_for_forage_DB_15july2015.xlsx")
head(SOf)
#URL_SOf <- "https://workspace.aoos.org/published/file/52211fd1e4b067e4402e7ca8/GWA_Benthic_Sea_Otter_Foraging_Data_2006-2012_28Aug2013.csv"
#SOfGet <- GET(URL_SOf)
#SOf1 <- content(SOfGet, as='text')
#SOf <- read.csv(file=textConnection(SOf1))

# 2013 data
SOf1 <- read_excel("C://Users//rblake//Documents//NCEAS//GoA Dynamics WG//GW_Nearshore Intertidal Data//Sea_Otter_Stuff//FINAL_2013_all_forage_prepped_for_forage_db8dec2015.xlsx", sheet="all2013")
head(SOf1)

# 2014 data
URL_SOf2 <- "https://workspace.aoos.org/published/file/e6bf00c8-1914-41c5-a5fe-4047ddad2bfb/NearshoreBenthicSystemsInGOA_SOP02_SeaOtterForageObservations2014_Data_29May2015.csv"
SOf2Get <- GET(URL_SOf2)
SOf2 <- content(SOf2Get, as='text')
SOf2 <- read.csv(file=textConnection(SOf2))
head(SOf2)

# merge 2002-2012 data with 2013 data
SOf_early <- merge(SOf, SOf1, all=TRUE)  
# clear up issues with date formatting
SOf_early <- SOf_early %>%
             select(-`otter utm_1`,-`otter utm_2`) %>%
             mutate_each(funs(as.factor), bout_date) %>% # change class of column
             mutate_each(funs(as.factor), start_time) %>% # change class of column
             mutate_each(funs(as.factor), end_time) # change class of column


# merge in the 2014 data with the rest
SOf_all <- merge(SOf_early, SOf2, all=TRUE)  
SOf_all$start_time <- substr(SOf_all$start_time, 12, 19)  # removes weird year digits added by read_excel()
SOf_all$end_time <- substr(SOf_all$end_time, 12, 19)  # removes weird year digits added by read_excel()


# cleaning all the data
SOf_A <- SOf_all %>% 
         rename(Region=area_cd) %>%
         mutate(preytype_cd = toupper(preytype_cd),
                YearSlash = sapply(strsplit(as.character(bout_date), split="/") , function(x) x[3]),
                YearDash = sapply(strsplit(as.character(bout_date), split="-") , function(x) x[1])) %>%
         rename(Year=YearSlash) %>%
         mutate(Year = ifelse((is.na(Year)),YearDash,Year),
                Region = revalue(Region, c(wpws="WPWS",npws="NPWS",kefj="KEFJ",katm="KATM")),
                Region = ifelse((specific_location_txt=="Surprise Bay-5-RI3/RI4" & Region=="KEFJ"),'WPWS',Region),
                
                Site_Name = ifelse((site_name %in% c("Iktua Bay, PWS_B08_RI_02") & Region=="WPWS"),'Iktua Bay',  
                            ifelse((site_name %in% c("Harris Bay, KEP_B05_RI_05") & Region=="KEFJ"),'Harris Bay',
                            ifelse((site_name %in% c("Nuka Passage, KEP_B05_RI_04") & Region=="KEFJ"),'Nuka Passage',
                            ifelse((site_name %in% c("Johnson Bay, PWS_B08_RI_04") & Region=="WPWS"),'Johnson Bay',
                            ifelse((site_name %in% c("Herring Bay, PWS_B08_RI_05") & Region=="WPWS"),'Herring Bay',
                            ifelse((site_name %in% c("Takli Island, AKP_B10_RI_05") & Region=="KATM"),'Takli Island', 
                            ifelse((site_name %in% c("Amalik Bay, AKP_B10_RI_04") & Region=="KATM"),'Amalik Bay',
                            ifelse((site_name %in% c("Nuka Bay, KEP_B05_RI_03") & Region=="KEFJ"),'Nuka Bay',
                            ifelse((site_name %in% c("McCarty Fjord, KEP_B05_RI_02") & Region=="KEFJ"),'McCarty Fjord',       
                                   
                            ifelse((specific_location_txt %in% c("Aialik Bay","Amalik Bay","Bettles Bay","Cedar Bay",
                                                                 "Chinitna Bay","Chisik Island","Disk Island",
                                                                 "Esther Passage","Galena Bay","Harris Bay","Herring Bay",
                                                                 "Herring Bay-Bear Cove","Herring Bay-Southwest",
                                                                 "Hogan Bay","Iktua Bay","Johnson Bay","Johnson Creek",
                                                                 "Kaflia Bay","Kinak Bay","Kukak Bay","McCarty Fjord",
                                                                 "Ninagiak Island","Northwest Bay","Nuka Bay","Nuka Passage",
                                                                 "Observation Island","Olsen Bay","Perry Island",
                                                                 "Polly Creek","Port Fidalgo","Simpson Bay","Takli Island",
                                                                 "Tukendni Bay","Unakwik Inlet","Whale Bay")),
                                                                  specific_location_txt,  
                             
                            ifelse((specific_location_txt %in% c("Nuka Island West","Nuka Island","Nuka","Beautiful Island",
                                                                 "islands west of Nuka", "Islands west of Nuka Island",
                                                                 "BEAUTIFUL-5-RI3","West Nuka","Nuka West Islands","nuka island",
                                                                 "nuka","NUKA BAY","beauty bay"
                                                                 ) & Region=="KEFJ"),'Nuka Bay', 
                                   
                            ifelse((specific_location_txt %in% c("West End Nuka Pass","Yalik Bay","Yalik Beach","North Nuka Pass",
                                                                 "Nuka Pass","Nuka-5-RI4","TONSINA-5-RI4","TONSINA","Tonsina",
                                                                 "Tonsina; L Island","Tonsina; tip of L Island","inside Tonsina",
                                                                 "Tonsina-5-RI4","Berger Bay Rocks"
                                                                 ) & Region=="KEFJ"),'Nuka Passage',
                                   
                            ifelse((specific_location_txt %in% c("James Lagoon","james lagoon","MCCARTY LAGOON-5-RI2","MCCARTY LAGOON",
                                                                 "McCarty-5-RI2","McCarty Lagoon","McCarty","Outside James Lagoon",
                                                                 "McCarty; James Lagoon","entrance mccarthy","James Lagoon; Outside",
                                                                 "McCarty-outside beach","mccarty","JAMES LAGOON"  
                                                                 ) & Region=="KEFJ"),'McCarty Fjord',
                                   
                            ifelse((specific_location_txt %in% c("Little Mink","Takli/Mink-10-RI5","little mink island","TAKLI/MINK Is.-10-RI5",
                                                                 "TAKLI/LITTLE MINK-10-RI5/RI4","TAKLI/MINK-10-RI4/5","Takli Island",
                                                                 "Takli-10-RI5","Takli/Ilktugitak-10-RI5","MINK IS.-10-05I","Little MInk",
                                                                 "takli","mink island","MINK ISLAND"
                                                                 ) & Region=="KATM"),'Takli Island', 
                            
                            ifelse((specific_location_txt %in% c("Amalik Bay-10-RI4","Amalik; .","Amalik; Little Mink","Amalik; Mink",
                                                                 "Amalik Bay, AKP_B10_RI_04 in Amalik Bay, obs done from deck of charter vessel",
                                                                 "Amalik; Ilktigadak","AMALIK/MINK ISL-10-RI5/RI4","ILKTUGIDAK"
                                                                 ) & Region=="KATM"),'Amalik Bay',
                                   
                            ifelse((specific_location_txt %in% c("KUKAK B-10-01I","KUKAK/DEVILS COVE-10-RI1","DEVIL'S COVE","Devils Cove",
                                                                 "Kukak/Yugnat-10-RI1","Kukak/Devil's Cove-10-RI1","Kukak; Devils cove",
                                                                 "Kukak"
                                                                 ) & Region=="KATM"),'Kukak Bay',
                                   
                            ifelse((specific_location_txt %in% c("Harris Bay Surf Beach","Harris Bay Spit","Otter Cove","Northwestern Lagoon",
                                                                 "NW Lagoon","Otter Cove Moraine","Harris Lagoon","Northwestern",
                                                                 "Sea Otter Cove-5-RI5","Otter cove-5-RI5","Outside Otter Cove",
                                                                 "Harris; near mussel site","Harris","OTTER COVE","Otter  Cove" 
                                                                 ) & Region=="KEFJ"),'Harris Bay', 
                                   
                            ifelse((specific_location_txt %in% c("N. Squirrel Island","South Squirrel","Clam Island","East Squirrel Passage",
                                                                 "North Squirrel Island/Johnson Bay","North Squirrel Is.","N. Squirrel Is.",
                                                                 "North Squirrel Island","SOUTH SQUIRREL","JOHNSON COVE","SQUIRREL",
                                                                 "JOHNSON BAY","S. SQUIRREL","JOHNSON","MOUTH OF JOHNSON BAY","N. SQUIRREL BAY",
                                                                 "n. squirrel","s. squirrel","JOHNSON BAY; KNIGHT ISL","N SQUIRREL" 
                                                                 ) & Region=="WPWS"),'Johnson Bay',
                                   
                            ifelse((specific_location_txt %in% c("Aialik-5-RI1","Pederson-5-RI1","Aialik/Squab Isl-5-RI1","Pederson/Aialik",
                                                                 "Pederson/ Aialik","Pedersen","AIALIK","Aialik"    
                                                                 ) & Region=="KEFJ"),'Aialik Bay',     
                                   
                            ifelse((specific_location_txt %in% c("IKTUA","Iktua Passage","iktua bay","IKTUA BAY","Surprise Bay-5-RI3/RI4"
                                                                 ) & Region=="WPWS"),'Iktua Bay',      
                                   
                            ifelse((specific_location_txt %in% c("KAFLIA/CAPE GULL-10-RI2"
                                                                 ) & Region=="KATM"),'Kaflia Bay',   
                                   
                            ifelse((specific_location_txt %in% c("HERRING BAY","bay of isles","BOI; KIM'S ROCK","BAY OF ISLES; KIM'S ROCK",
                                                                 "Bay of Isles S. Bite","Bay of Isles","Eagles Nest Isl/BOI","BOI","kims rock",
                                                                 "boi","BAY OF ISLES","KIMS RK ","EAGLE NEST IS/","EAGLE NEST ISLAND","OUTSIDE BOI",
                                                                 "KIM'S ROCK","EAGLE'S NEST"  
                                                                 ) & Region=="WPWS"),'Herring Bay',       
                            
                            ifelse((specific_location_txt %in% c("WHALE BAY","whale bay","ELESHANSKY COVE; WHALE BAY"
                                                                 ) & Region=="WPWS"),'Whale Bay',  
                                   
                            ifelse((specific_location_txt %in% c("devil's cove","KUKAK/DEVILS COVE-10-RI1","Devil's Cove"
                                                                 ) & Region=="KATM"),'Kukak Bay',   
                                   
                            ifelse((specific_location_txt %in% c("GALENA BAY") & Region=="EPWS"),'Galena Bay',    
                                   
                            ifelse((specific_location_txt %in% c("OLSON BAY") & Region=="EPWS"),'Olsen Bay',       
                                   
                            ifelse((specific_location_txt %in% c("bettles") & Region=="NPWS"),'Bettles Bay',     
                                   
                            ifelse((specific_location_txt %in% c("SIMPSON BAY","MOUTH OF SIMPSON BAY"
                                                                 ) & Region=="EPWS"),'Simpson Bay',           
                            
                            ifelse((specific_location_txt %in% c("DISKWEST","NDISK") & Region=="WPWS"),'Disk Island',       
                                   
                                   "Unknown")))))))))))))))))))))))))))))
                 ) %>%
        filter(Year %in% c(2010,2011,2012,2013,2014,2015),
               Site_Name != "Unknown") %>%    # removes observations not matched to a Gulf Watch Site
        select(Region,Site_Name,Year,obs_lat,obs_long,otter_lat,otter_long,preytype_cd,prey_qty,
               preysize_cd,prey_size_cm,not_eaten_reason_cd,not_eaten_pct,prey_name,Latin_name,
               `prey kg`)
        
# PWS Mystery locations (removed from the dataset because not matched to a Gulf Watch Site): 
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
                
# Calculate proportion of biomass provided by clams, mussels, crabs, and other  
# paste in prey name info 
BmCalc_sub <- BmCalc[,c(1:3,5:7)]
BmCalc_sub <- rename(BmCalc_sub, preytype_cd=Prey)
BmCalc_sub$preytype_cd <- as.character(BmCalc_sub$preytype_cd)
SOf_bm1 <- merge(SOf_A, BmCalc_sub, by="preytype_cd", all=TRUE) # merge prey name info into dataset

# paw size code definitions (from Dan Monson via e-mail)
PawSize <- data.frame(preysize_cd=c("1a","1b","1c","2a","2b","2c","3a","3b","3c","4z","9z"), 
                      prey_size_cm=c(1.3,2.6,3.9,6.5,7.8,9.1,11.7,13,14.3,15.6,35)) #9z is estimated at 35cm

SOf_bm <- merge(SOf_bm1, PawSize, by=c("preysize_cd"), all.y=TRUE)  # merge in paw sizes
SOf_bm <- SOf_bm %>%
          select(-prey_size_cm.x) %>%
          rename(prey_size_cm = prey_size_cm.y) %>%
          mutate(PreySize_mm = prey_size_cm/0.1) %>%
          select(Region,Site_Name,Year,obs_lat,obs_long,otter_lat,otter_long,preytype_cd,
                 prey_qty,preysize_cd,prey_size_cm,PreySize_mm,everything()) %>%
          filter(!is.na(Region),!is.na(Type),!is.na(prey_qty))

#write.csv(SOf_bm, file = "OtterForage_ForBryce.csv", row.names=FALSE)

Prey_PBmss <- function(df, prey, preytypebmss_colname, propbmss_colname){
              # sum of all biomass per site per year
              B <- df %>%
                   rename(PreyType=Type) %>%
                   mutate_(.dots = setNames(list(~df[,21]*
                                                  df$PreySize_mm^df[,22]), 
                                                  "biomass_gWW")) %>%
                   mutate(Ttlbiomass_gWW = biomass_gWW*prey_qty) %>%
                   filter(PreyType != "Unknown") %>%
                   group_by(Region,Site_Name,Year) %>%
                   summarise_(.dots = setNames(list(~sum(Ttlbiomass_gWW)), 
                                               "SiteSumBmss_gWW")) %>%
                   ungroup()
              # sum of biomass per type per site per year
              C <- df %>%
                   rename(PreyType=Type) %>%
                   mutate_(.dots = setNames(list(~df[,21]*
                                                  df$PreySize_mm^df[,22]), 
                                                  "biomass_gWW")) %>%
                   mutate(Ttlbiomass_gWW = biomass_gWW*prey_qty) %>%
                   filter(PreyType != "Unknown") %>%
                   group_by(Region,Site_Name,Year,PreyType) %>%
                   summarise_(.dots = setNames(list(~sum(Ttlbiomass_gWW)), 
                                               preytypebmss_colname)) %>%
                   ungroup() %>%
                   select_("Region", "Site_Name", "Year", "PreyType", preytypebmss_colname) %>%
                   arrange(Region,Site_Name,Year)
                
              # proportion of biomass for each prey type per site per year
              D <- df %>%
                   rename(PreyType=Type) %>%
                   filter(PreyType != "Unknown") %>%
                   full_join(C, by=c("Region","Site_Name","Year","PreyType")) %>%
                   arrange(Region,Site_Name,Year)
              
              E <- D %>%    
                   full_join(B, by=c("Region","Site_Name","Year")) %>%
                   group_by(Region,Site_Name,Year,PreyType) %>%  
               #    mutate_(.dots = setNames(list(~preytypebmss_colname/SiteSumBmss_gWW),
               #                             propbmss_colname)) %>%
                  
               #    mutate_(.dots = setNames(list(interp(~preytypebmss_colname/SiteSumBmss_gWW)),
              #                              propbmss_colname)) %>%
                  
               
                   ungroup() %>%
                   select_("Region", "Site_Name", "Year", "PreyType", preytypebmss_colname,
                           "SiteSumBmss_gWW", propbmss_colname) %>%
                   arrange(Region,Site_Name,Year)
              
              # subset on prey type
              G <- E %>%
                   filter_(.dots=list(~PreyType == prey)) %>%
                   select(-SiteSumBmss_gWW,-PreyType)
                   distinct()
         
              return(G)
              }
  
# calculate for each prey type
SOf_Crab_Bmss <- Prey_PBmss(SOf_bm, "Crab", "SOf_CrabSumBmss_gWW", "SOf_CrabPropBmss")

SOf_Clam_Bmss <- Prey_PBmss(SOf_bm, "Clam", "SOf_ClamSumBmss_gWW", "SOf_ClamPropBmss")
  
SOf_Urch_Bmss <- Prey_PBmss(SOf_bm,  "Urchin", "SOf_UrchinSumBmss_gWW", "SOf_UrchinPropBmss")
  
SOf_Muss_Bmss <- Prey_PBmss(SOf_bm, "Mussel", "SOf_MusselSumBmss_gWW", "SOf_MusselPropBmss")
  
SOf_Star_Bmss <- Prey_PBmss(SOf_bm, "Star", "SOf_StarSumBmss_gWW", "SOf_StarPropBmss")
  
SOf_Snail_Bmss <- Prey_PBmss(SOf_bm, "Snail", "SOf_SnailSumBmss_gWW", "SOf_SnailPropBmss")
  
SOf_Chi_Bmss <- Prey_PBmss(SOf_bm, "Chiton", "SOf_ChitonSumBmss_gWW", "SOf_ChitonPropBmss")
  
SOf_Octo_Bmss <- Prey_PBmss(SOf_bm, "Octopus", "SOf_OctopusSumBmss_gWW", "SOf_OctopusPropBmss")
  
SOf_Worm_Bmss <- Prey_PBmss(SOf_bm, "Worm", "SOf_WormSumBmss_gWW", "SOf_WormPropBmss")
  
SOf_Other_Bmss <- Prey_PBmss(SOf_bm, "Other", "SOf_OtherSumBmss_gWW", "SOf_OtherPropBmss")
  
  
  
  
# browser() function for debugging functions (hehe)









