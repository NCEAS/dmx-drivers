#########################################################################
##### GOA Dynamics Working Group                                    #####
##### Benthic Nearshore Group - Dataframe Assembly script           #####
##### Created by Rachael Blake on Sept. 21, 2015                    #####
#####                                                               #####
##### This script assembles one large dataframe from data cleaned   #####
##### by each individual parameter cleaning script.                 #####
#########################################################################

## Load packages needed for this script (order matters)
library(httr) ; library(plyr) ; library(dplyr) ; library(XML) ; library(curl) ; library(rvest)
library(tidyr) ; library(stringr)

## Set the file path parameters by entering your Sys.info()["user"] and 
## local filepath to the dmx-drivers repo
dir_dmx_d = c('rblake'='C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/dmx-drivers'#,
              #'yourusername'='enter/your/path/to/repo'
              )[Sys.info()["user"]]

## --------
## Steps for making the large dataframe with all data:
## 1) run each data cleaning script to generate individual parameter data frames
## 2) create empty data frame with site, region, and year columns populated
## 3) merge all data frames with BenNear empty dataframe: BenNear=merge(BenNear,newData,all.x=T) 
## --------

## --------
## Source and run each data cleaning script
## This function sources all files in a given folder/directory.  It comes from the source()
## help page documentation.  

#' Source Directory Function
#' @param path the path to the folder where you want to source files
#' @param trace 
#' @return
#' @export
#' @examples
sourceDir <- function(path, trace=TRUE) {
             for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
                  if(trace) cat(nm,":")
                  source(file.path(path, nm))
                  if(trace) cat("\n")
                  }
}

## This runs the above sourcing function on the files located in the user's local copy of
## the dmx-drivers repository.
## The result is that all the cleaning scripts in the directory are run and their dataframes are
## available.
if(Sys.info()["user"]!='rblake'){
   sourceDir("Benthic_Nearshore_Drivers/Data_Cleaning_Scripts_DMX_Benthic_Nearshore")
}else{sourceDir(file.path(dir_dmx_d, 
               "Benthic_Nearshore_Drivers/Data_Cleaning_Scripts_DMX_Benthic_Nearshore"))
}
## --------
 
## -------- 
## Create empty data frame with Year, Region, Site, and Quadrat columns
BenNearSites <- data.frame('Site_Name'=c("Aialik Bay","Amalik Bay","Bettles Bay","Cedar Bay",
                                         "Chinitna Bay","Chisik Island","Disk Island",
                                         "Esther Passage","Galena Bay","Harris Bay","Herring Bay",
                                         "Herring Bay-Bear Cove","Herring Bay-Southwest",
                                         "Hogan Bay","Iktua Bay","Johnson Bay","Johnson Creek",
                                         "Kaflia Bay","Kinak Bay","Kukak Bay","McCarty Fjord",
                                         "Ninagiak Island","Northwest Bay","Nuka Bay","Nuka Passage",
                                         "Observation Island","Olsen Bay","Perry Island",
                                         "Polly Creek","Port Fidalgo","Simpson Bay","Takli Island",
                                         "Tukendni Bay","Unakwik Inlet","Whale Bay")
                           )

BenNear <- BenNearSites %>% 
           mutate(Region = ifelse((Site_Name %in% c("Galena Bay","Observation Island",
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
                                                    "Northwest Bay","Whale Bay")),'WPWS',"")))))),
                  Lat = plyr::mapvalues(Site_Name, c("Aialik Bay","Amalik Bay","Bettles Bay","Cedar Bay",
                                        "Chinitna Bay","Chisik Island","Disk Island",
                                        "Esther Passage","Galena Bay","Harris Bay","Herring Bay",
                                        "Herring Bay-Bear Cove","Herring Bay-Southwest",
                                        "Hogan Bay","Iktua Bay","Johnson Bay","Johnson Creek",
                                        "Kaflia Bay","Kinak Bay","Kukak Bay","McCarty Fjord",
                                        "Ninagiak Island","Northwest Bay","Nuka Bay","Nuka Passage",
                                        "Observation Island","Olsen Bay","Perry Island",
                                        "Polly Creek","Port Fidalgo","Simpson Bay","Takli Island",
                                        "Tukendni Bay","Unakwik Inlet","Whale Bay"),
                                        c('59.876640','58.079222','60.954960', '60.966140','59.875902',
                                          '60.174123','60.504330','60.925700','60.932510','59.737660',
                                          '60.459890','60.469310','','60.201970','60.130040','60.339950',
                                          '60.000127','58.256944','58.186556','58.316583','59.508530',
                                          '58.454510','60.555430','59.537230','59.420710','60.602070', 
                                          '60.731190','60.677100','60.291554', '60.862960','60.680820',
                                          '58.063500','60.225920', '60.949480','60.226610')),
                  Long = plyr::mapvalues(Site_Name, c("Aialik Bay","Amalik Bay","Bettles Bay","Cedar Bay",
                                         "Chinitna Bay","Chisik Island","Disk Island",
                                         "Esther Passage","Galena Bay","Harris Bay","Herring Bay",
                                         "Herring Bay-Bear Cove","Herring Bay-Southwest",
                                         "Hogan Bay","Iktua Bay","Johnson Bay","Johnson Creek",
                                         "Kaflia Bay","Kinak Bay","Kukak Bay","McCarty Fjord",
                                         "Ninagiak Island","Northwest Bay","Nuka Bay","Nuka Passage",
                                         "Observation Island","Olsen Bay","Perry Island",
                                         "Polly Creek","Port Fidalgo","Simpson Bay","Takli Island",
                                         "Tukendni Bay","Unakwik Inlet","Whale Bay"),
                                         c('-149.632890','-154.466000','-148.299420', '-147.394910',
                                           '-152.927586','-152.593689','-147.654660','-148.058720',
                                           '-146.665010','-149.958370','-147.717530','-147.709290',
                                           '','-147.759840','-147.998280','-147.834820','-152.623964',
                                           '-154.197694','-154.465750','-154.206583','-150.341780',
                                           '-154.014360','-147.611770','-150.607130','-150.646960',
                                           '-145.730620','-146.188310', '-147.916890','-152.404880',
                                           '-146.230090','-145.878720','-154.484056','-152.550853',
                                           '-147.594440','-148.251050'))
                  ) %>%
           arrange(Region, Site_Name)

start_year <- 2000
end_year <- 2015
BenNear <- BenNear[rep(seq_len(nrow(BenNear)), each=((end_year-start_year)+1)),]   # repeats data frame the number of years
BenNear$Year=rep(c(start_year:end_year))   # adds the year column with the years filled in
                      
BenNear <- BenNear[rep(seq_len(nrow(BenNear)), each=12),]   # repeats data frame the number of quadrats
BenNear$Quadrat=rep(c(1:12))   # adds the quadrat column with the quad # filled in
## --------

## --------
## Merge data columns generated by data cleaning scripts into one large data frame

## List of all the parameter dataframes that are the outputs of the cleaning scripts
data_list <- list(ENSO_ann,           # ENSO anomaly annual
                  ENSO_spr,           # ENSO anomaly spring
                  ENSO_fal,           # ENSO anomaly fall
                  PDO_ann_seasn,      # PDO annual
                  npgo_annual,        # NPGO annual
                  upanom_ann,         # Upwelling anomalies annual
                  upanom_fal,         # Upwelling anomalies fall
                  upanom_spr,         # Upwelling anomalies spring
                  WTmp_mn_ann_hobo,   # Annual mean water temp from HOBO loggers 
                  WaterTmp_Ann,       # Annual mean water temp from buoys
                  WaterTmp_Winter,    # Winter mean water temp from buoys
                  WaterTmp_WAnom,     # Water Temp Anomoly Winter Mean from buoys
                  WaterTmp_SAnom,     # Water Temp Anomoly Spring Mean frp, buoys
                  #Waves_Ann,         # Annual mean significant wave hgt and period from buoys
                  #Waves_Winter,      # Winter mean significant wave hgt and period from buoys
                  #Wind_Ann,          # Annual mean wind speed and direction from buoys
                  #Wind_Winter,       # Winter mean wind speed and direction from buoys
                  Phy_spr,            # Phytoplankton - Seward Line, spring
                  Phy_fal,            # Phytoplankton - Seward Line, fall
                  Phy_yr,             # Phytoplankton - Seward Line, annual
                  #SatChl_df,         # Chla - Satellite annual
                  #SST,               # SST - Seward Line
                  Wlk_GOA,            # Whelks (Nucella sp.) abundance (n/m2)
                  #SS_GOA,            # Sea Stars abundance
                  BS_IA,              # Bare Substrate percent cover
                  b_IA,               # Barnacles percent cover
                  ms_IA,              # Mussels percent cover
                  Fd_IA,              # Fucus percent cover
                  Am_IA,              # Alaria percent cover
                  No_IA,              # Odonthalia / Neorhodomela sp. percent cover
                  BAann_IA,           # Brown Algae Annual percent cover
                  GAann_IA,           # Green Algae Annual percent cover
                  RAann_IA,           # Red Algae Annual percent cover
                  RAper_IA,           # Red Algae Perennial percent cover
                  RAall_IA,           # Red Algae Total percent cover
                  #ElG,               # Eelgrass percent cover
                  OyC_GOA,            # BLOY breeding adults abundance
                  # ,                 # BLOY diet prey size
                  # ,                 # BLOY diet prey abundance
                  # ,                 # BLOY diet proportion biomass
                  #BLOYAbun,          # BLOY abundance
                  #HADUAbun,          # HADU abundance
                  #BAGOAbun,          # BAGO abundance
                  #Leuk_Abun_Size,    # Leukoma abundance, mean size, mean bimoass
                  #Maco_Abun_Size,    # Macoma abundance, mean size, mean biomass
                  #Saxi_Abun_Size,    # Saxidomus abundance, mean size, mean biomass
                  #SOf_Crab_Bmss,     # Sea Otter forage crab biomass
                  #SOf_Clam_Bmss,     # Sea Otter forage clam biomass
                  #SOf_Urch_Bmss,     # Sea Otter forage urchin biomass
                  #SOf_Muss_Bmss,     # Sea Otter forage mussel biomass
                  #SOf_Star_Bmss,     # Sea Otter forage sea star biomass
                  #SOf_Snail_Bmss,    # Sea Otter forage snail biomass
                  #SOf_Chi_Bmss,      # Sea Otter forage chiton biomass
                  #SOf_Octo_Bmss,     # Sea Otter forage octopus biomass
                  #SOf_Worm_Bmss,     # Sea Otter forage worm biomass
                  #SOf_Other_Bmss,    # Sea Otter forage "other" biomass
                  #SOAD,              # Sea Otter mean carcass tooth age; proportion of prime age
                  #SOPop,             # Sea Otter population estimates
                  REG,                # Sea Otter energy recovery (kcal/min)
                  FWDisc_Spring,      # Freshwater Discharge Spring Mean
                  FWDisc_Fall,        # Freshwater Discharge Fall Mean
                  FWDisc_Yearly      # Freshwater Discharge Annual Mean
)

## Function that merges all the dataframes 
#' Merge Dataframe Function
#' @param empty_df 
#' @param param_list 
#' @return
#' @export
#' @examples
merge_dfs <- function(empty_df, param_list){
                      full_df <- for (i in 1:length(param_list)) {
                                 merge(empty_df, param_list[i], all.x=T)
                                 }
                      return(full_df)
             }

## Apply the function to all the entire list of parameter dataframes
BenNear_test <- merge_dfs(empty_df=BenNear, param_list=data_list)



BenNear_test2 <- Reduce(function(BenNear,y) merge(BenNear,y, all.x=T), data_list)

###########################


BenNear <- arrange(BenNear,Region,Site_Name,Year,Quadrat)
## --------

## Optional: Write data frame to a CSV
#write.csv(BenNear, file = "BenthicNearshore_MusselQuestData.csv", row.names=FALSE)


##############################################################################################









