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
SOS <- SOs %>%
       rename(Year=YEAR, ToothAge=TOOTHAGE) %>%
  #     filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%
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
       filter(!Region=="unknown") %>%  # removing samples with no location information
       mutate(SITE = revalue(SITE, c("."=NA)),
              AGECLASS = revalue(AGECLASS, c("."=NA)),
              ToothAge = revalue(ToothAge, c("."=NA)),
              SEX = revalue(SEX, c("."=NA)),
              CARCCOL = revalue(CARCCOL, c("."=NA)),
              SKULCOL = revalue(SKULCOL, c("."=NA)),
              MAND_COL = revalue(MAND_COL, c("."=NA)),
              WHIS_COL = revalue(WHIS_COL, c("."=NA)),
              BAC_COL = revalue(BAC_COL, c("."=NA)),
              BAC_LNTH = revalue(BAC_LNTH, c("."=NA)),
              TOOTHCOL = revalue(TOOTHCOL, c("."=NA)),
              LAT_DD = revalue(LAT_DD, c("."=NA)),
              LON_DD = revalue(LON_DD, c("."=NA)),
              TOOTH = revalue(TOOTH, c("."=NA)),
              CARCCOND = revalue(CARCCOND, c("."=NA)),
              CC.From.Matson = revalue(CC.From.Matson, c("."=NA)),
              Age.Range = revalue(Age.Range, c("."=NA))
              ) %>%
       rename(SeaOtt_CarcToothAge=ToothAge) %>%
       select(Year, Region, SeaOtt_CarcToothAge)
  

####
# FUNCTION to calculate mean carcass tooth age per site and
# proportion of prime age (defined as 2-8 years old)
####
MnPrimAge <- function(df){
             MnAge <- df %>%
                      filter(!is.na(SeaOtt_CarcToothAge)) %>%
                      mutate_each(funs(as.numeric), SeaOtt_CarcToothAge) %>% # change class of column 
                      group_by(Year, Region) %>%
                      summarise(SOtt_MnCarcToothAge = mean(SeaOtt_CarcToothAge)) %>%
                      ungroup()
             
             u <- df %>%
                  filter(!is.na(SeaOtt_CarcToothAge)) %>%
                  # count total number per region
                  count(Year, Region) %>%
                  rename(Total_Count=n)
             v <- df %>%
                  filter(!is.na(SeaOtt_CarcToothAge)) %>%
                  # subset those 2-8 years old and count how many
                  filter(SeaOtt_CarcToothAge %in% c(2:8)) %>%
                  count(Year,Region) %>%
                  rename(Prime_Count=n)
             w <- full_join(u,v, by=c("Year","Region")) %>%
                  replace(is.na(.), 0)
             
             PropPrime <- w %>%  
                          # calculate proportion
                          mutate(SOtt_PropPrimeCarcTthAge = (Prime_Count/Total_Count)*100) %>%
                          select(-Total_Count, -Prime_Count)
             
             SOC <- full_join(MnAge,PropPrime, by=c("Year","Region"))
             return(SOC)
             }



SOAD <- MnPrimAge(SOS)


#  x <- readClipboard()



