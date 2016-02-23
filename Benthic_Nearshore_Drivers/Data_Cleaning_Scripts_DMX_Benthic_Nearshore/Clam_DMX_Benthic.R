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
# Clam data   2007 - 2015        # note quadrats are 0.25m squared in size
URL_C <- "https://workspace.aoos.org/published/file/ddc1f9bc-23c3-4c88-aa71-181001138171/NearshoreBenthicSystemsInGOA_SOP06_InvertsGravelSandBeaches_2007-2015SpeciesSize_Data_11Feb2016.csv"
CGet <- GET(URL_C)
C1 <- content(CGet, as='text')
Cl <- read.csv(file=textConnection(C1))
head(Cl)

# biomass conversion from Ben Weitzman based on Oftedal et al. 2007 nutritional analyses
URL_Bm <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uN2FrdzIzLS1FZUk"
BmGet <- GET(URL_Bm)
Bm1 <- content(BmGet, as='text')
BmCalc <- read.csv(file=textConnection(Bm1))
head(BmCalc)

# clean the data          
Clam <- Cl %>%
        rename(Site_Name=site.name, Quadrat=quad.,Size_mm=size..mm.) %>%
        mutate(Year = sapply(strsplit(as.character(date), split="/") , function(x) x[3]),
               Year = revalue(Year, c("3013"="2013")),
               Site_Name = revalue(Site_Name,c("Ninagiak"="Ninagiak Island",
                                               "Herring Bay- Southwest"="Herring Bay-Southwest",
                                               "Galena"="Galena Bay", "Fidalgo"="Port Fidalgo",
                                               "Olsen"="Olsen Bay", "Simpson"="Simpson Bay",
                                               "Observation"="Observation Island")),
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
         arrange(Site_Name, Year, Quadrat) #%>%
   #     filter(Year %in% c(2010,2011,2012,2013,2014,2015))

# pull out biomass conversion info for species of interest
BM <- BmCalc[grepl("(Macoma|Saxidomus|Leukoma)", BmCalc$Latin),]

#####
# Function to calculate metrics of interest        
BiV_Abun_Size <- function(df, genus, abun_column_name, size_column_name, biom_column_name) { 
                 # subset df based on genus
                 df2 <- df[grepl(genus, df$spp.name),] 
                 # calculate the abundance (count number of rows)
                 A <- df2 %>%
                      count(Region, Site_Name, Year, Quadrat) %>%
                      mutate_(.dots= setNames(list(~n*4), abun_column_name)) %>%
                      select_("Region", "Site_Name", "Year", "Quadrat", abun_column_name)
                 # calculate the mean size (average per quadrat)
                 B <- df2 %>%
                      filter(!is.na(Size_mm), Size_mm != 999) %>%
                      group_by(Region,Site_Name,Year,Quadrat) %>% 
                      summarise_(.dots = setNames(list(~mean(Size_mm)), size_column_name)) %>%  # mean of elevations together
                      ungroup() %>%
                      select_("Region", "Site_Name", "Year", "Quadrat", size_column_name)
                 # bind the two data frames together
                 C <- merge(A, B, by=c("Region","Site_Name","Year","Quadrat"))
                 # calculate biomass 
                 # from size (mm) to biomass (grams wet wt.): Biomass = fxna * size ^ fxnb
                 d <- df2 %>%
                      filter(!is.na(Size_mm), Size_mm != 999) 
                 
                 D <- d %>%
                      mutate_(.dots= setNames(list(~BM[grepl(genus,BM$Latin),6]*
                                                    d$Size_mm^BM[grepl(genus,BM$Latin),7]), 
                                                    "biomass_gWW")) %>%
                      group_by(Region,Site_Name,Year,Quadrat) %>%
                      summarise_(.dots = setNames(list(~mean(biomass_gWW)), biom_column_name)) %>%
                      ungroup() %>%
                      select_("Region", "Site_Name", "Year", "Quadrat", biom_column_name)
                 # bind biomass data to other dataframes
                 E <- merge(C,D, by=c("Region","Site_Name","Year","Quadrat"))
                 return(E)
                 }

#####
#####
#####
# FUNCTION for adding zeros for samples where species were not observed
# Adapted From http://stackoverflow.com/questions/10438969/fastest-way-to-add-rows-for-missing-values-in-a-data-frame

AddZeros2 <- function(df, genus, abun_column_name, size_column_name, biom_column_name){
             # run function PerCovCalc here and make the output (df1) available to the pipe below
             df1 <- BiV_Abun_Size(df, genus, abun_column_name, size_column_name, biom_column_name)  
             # create data frame with one column with 12 quadrats for each unique combination of Site_Name and Year
             z <- df1 %>%
                  mutate(Site_Year = paste(Site_Name, Year, Region, sep="/")) %>%
                  expand(Site_Year, Quadrat)  
             # insert rows for missing quadrat values, ie, not found in c(1:12),     
             u <- df1 %>%
                  mutate(Site_Year = paste(Site_Name, Year, Region, sep="/")) %>%
                  full_join(z, by=c("Site_Year","Quadrat")) %>% 
                  arrange(Site_Year, Quadrat) %>%
             # and copy and paste Site_Name and Year  
                  mutate(Site_Name = sapply(strsplit(as.character(Site_Year), split="/"), function(x) x[1]),
                         Year = sapply(strsplit(as.character(Site_Year), split="/"), function(x) x[2]),
                         Region = sapply(strsplit(as.character(Site_Year), split="/"), function(x) x[3])) 
             # and replace NA with zero in "new_column_name" column
             u[is.na(u[,abun_column_name]), abun_column_name] <- 0
             u[is.na(u[,size_column_name]), size_column_name] <- 0
             u[is.na(u[,biom_column_name]), biom_column_name] <- 0
             # get rid of Site_Year column
             u <- select(u,-Site_Year)
             return(u)
             }
#####
#####







# Leukoma staminea abundance
Leuk_Abun_Size <- AddZeros2(Clam,"Leukoma",
                            "Leukoma_Abun_m2","Leukoma_MnSize_mm","Leukoma_MnBmss_gWW")    

# Macoma spp.
Maco_Abun_Size <- AddZeros2(Clam,"Macoma",
                            "Macoma_Abun_m2","Macoma_MnSize_mm","Macoma_MnBmss_gWW")
  
#Saxidomus gigantea
Saxi_Abun_Size <- AddZeros2(Clam,"Saxidomus",
                            "Saxidomus_Abun_m2","Saxidomus_MnSize_mm","Saxidomus_MnBmss_gWW")


  




