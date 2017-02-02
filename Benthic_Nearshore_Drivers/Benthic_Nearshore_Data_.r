#########################################################################
##### GOA Dynamics Working Group                                    #####
##### Benthic Nearshore Group - Data Assembly script                #####
##### Created by Rachael Blake on Sept. 21, 2015                    #####
#########################################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)


 dir_dmx_d = c('rblake'='C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/dmx-drivers'
             # collobrators enter your Sys.info()["user"] and local filepaths to our dmx-drivers repo
             )[Sys.info()["user"]]


## Steps for adding data columns:
## 1) run each data cleaning script to generate data frames
## 2) create empty data frame
## 3) merge all data frames with CoPrct dataframe:   
##        CoPrct=merge(CoPrct,newData,all.x=T)  


# Source and run each data cleaning script
sourceDir <- function(path, trace=TRUE) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
       if(trace) cat(nm,":")
       source(file.path(path, nm))
       if(trace) cat("\n")
    }
}


if(Sys.info()["user"]!='rblake'){
   sourceDir("Benthic_Nearshore_Drivers/Data_Cleaning_Scripts_DMX_Benthic_Nearshore")
}else{sourceDir(file.path(dir_dmx_d, 
               "Benthic_Nearshore_Drivers/Data_Cleaning_Scripts_DMX_Benthic_Nearshore"))
}



# Create empty data frame with Year, Region, Site, and Quadrat columns
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
                  Lat = ifelse((Site_Name=="Aialik Bay"),'59.876640',
                        ifelse((Site_Name=="Amalik Bay"),'58.079222',
                        ifelse((Site_Name=="Bettles Bay"),'60.954960', 
                        ifelse((Site_Name=="Cedar Bay"),'60.966140',
                        ifelse((Site_Name=="Chinitna Bay"),'59.875902',
                        ifelse((Site_Name=="Chisik Island"),'60.174123',
                        ifelse((Site_Name=="Disk Island"),'60.504330',
                        ifelse((Site_Name=="Esther Passage"),'60.925700',
                        ifelse((Site_Name=="Galena Bay"),'60.932510',
                        ifelse((Site_Name=="Harris Bay"),'59.737660',
                        ifelse((Site_Name=="Herring Bay"),'60.459890',
                        ifelse((Site_Name=="Herring Bay-Bear Cove"),'60.469310',
                        ifelse((Site_Name=="Herring Bay-Southwest"),'',
                        ifelse((Site_Name=="Hogan Bay"),'60.201970',
                        ifelse((Site_Name=="Iktua Bay"),'60.130040',
                        ifelse((Site_Name=="Johnson Bay"),'60.339950',
                        ifelse((Site_Name=="Johnson Creek"),'60.000127',
                        ifelse((Site_Name=="Kaflia Bay"),'58.256944',
                        ifelse((Site_Name=="Kinak Bay"),'58.186556',
                        ifelse((Site_Name=="Kukak Bay"),'58.316583',
                        ifelse((Site_Name=="McCarty Fjord"),'59.508530',
                        ifelse((Site_Name=="Ninagiak Island"),'58.454510',
                        ifelse((Site_Name=="Northwest Bay"),'60.555430',
                        ifelse((Site_Name=="Nuka Bay"),'59.537230',
                        ifelse((Site_Name=="Nuka Passage"),'59.420710',
                        ifelse((Site_Name=="Observation Island"),'60.602070', 
                        ifelse((Site_Name=="Olsen Bay"),'60.731190',  
                        ifelse((Site_Name=="Perry Island"),'60.677100',  
                        ifelse((Site_Name=="Polly Creek"),'60.291554',  
                        ifelse((Site_Name=="Port Fidalgo"),'60.862960',  
                        ifelse((Site_Name=="Simpson Bay"),'60.680820',  
                        ifelse((Site_Name=="Takli Island"),'58.063500',  
                        ifelse((Site_Name=="Tukendni Bay"),'60.225920',  
                        ifelse((Site_Name=="Unakwik Inlet"),'60.949480',  
                        ifelse((Site_Name=="Whale Bay"),'60.226610', ""))))))))))))))))))))))))))))))))))),
                  Long = ifelse((Site_Name=="Aialik Bay"),'-149.632890',
                         ifelse((Site_Name=="Amalik Bay"),'-154.466000',
                         ifelse((Site_Name=="Bettles Bay"),'-148.299420', 
                         ifelse((Site_Name=="Cedar Bay"),'-147.394910',
                         ifelse((Site_Name=="Chinitna Bay"),'-152.927586',
                         ifelse((Site_Name=="Chisik Island"),'-152.593689',
                         ifelse((Site_Name=="Disk Island"),'-147.654660',
                         ifelse((Site_Name=="Esther Passage"),'-148.058720',
                         ifelse((Site_Name=="Galena Bay"),'-146.665010',
                         ifelse((Site_Name=="Harris Bay"),'-149.958370',
                         ifelse((Site_Name=="Herring Bay"),'-147.717530',
                         ifelse((Site_Name=="Herring Bay-Bear Cove"),'-147.709290',
                         ifelse((Site_Name=="Herring Bay-Southwest"),'',
                         ifelse((Site_Name=="Hogan Bay"),'-147.759840',
                         ifelse((Site_Name=="Iktua Bay"),'-147.998280',
                         ifelse((Site_Name=="Johnson Bay"),'-147.834820',
                         ifelse((Site_Name=="Johnson Creek"),'-152.623964',
                         ifelse((Site_Name=="Kaflia Bay"),'-154.197694',
                         ifelse((Site_Name=="Kinak Bay"),'-154.465750',
                         ifelse((Site_Name=="Kukak Bay"),'-154.206583',
                         ifelse((Site_Name=="McCarty Fjord"),'-150.341780',
                         ifelse((Site_Name=="Ninagiak Island"),'-154.014360',
                         ifelse((Site_Name=="Northwest Bay"),'-147.611770',
                         ifelse((Site_Name=="Nuka Bay"),'-150.607130',
                         ifelse((Site_Name=="Nuka Passage"),'-150.646960',
                         ifelse((Site_Name=="Observation Island"),'-145.730620', 
                         ifelse((Site_Name=="Olsen Bay"),'-146.188310',  
                         ifelse((Site_Name=="Perry Island"),'-147.916890',  
                         ifelse((Site_Name=="Polly Creek"),'-152.404880',  
                         ifelse((Site_Name=="Port Fidalgo"),'-146.230090',  
                         ifelse((Site_Name=="Simpson Bay"),'-145.878720',  
                         ifelse((Site_Name=="Takli Island"),'-154.484056',  
                         ifelse((Site_Name=="Tukendni Bay"),'-152.550853',  
                         ifelse((Site_Name=="Unakwik Inlet"),'-147.594440',  
                         ifelse((Site_Name=="Whale Bay"),'-148.251050', "")))))))))))))))))))))))))))))))))))   
                  ) %>%
           arrange(Region)

BenNear <- BenNear[rep(seq_len(nrow(BenNear)), each=16),]   # repeats data frame the number of years
BenNear$Year=rep(c(2000:2015))   # adds the year column with the years filled in
                      
BenNear <- BenNear[rep(seq_len(nrow(BenNear)), each=12),]   # repeats data frame the number of quadrats
BenNear$Quadrat=rep(c(1:12))   # adds the quadrat column with the quad # filled in
                         

# Merge in data columns generated by data cleaning scripts into one large data frame
BenNear <- merge(BenNear,ENSO_ann,all.x=T)        # ENSO anomaly annual
BenNear <- merge(BenNear,ENSO_spr,all.x=T)        # ENSO anomaly spring
BenNear <- merge(BenNear,ENSO_fal,all.x=T)        # ENSO anomaly fall
BenNear <- merge(BenNear,PDO_ann_seasn,all.x=T)   # PDO annual
BenNear <- merge(BenNear,npgo_annual,all.x=T)     # NPGO annual
BenNear <- merge(BenNear,upanom_ann,all.x=T)      # Upwelling anomalies annual
BenNear <- merge(BenNear,upanom_fal,all.x=T)      # Upwelling anomalies fall
BenNear <- merge(BenNear,upanom_spr,all.x=T)      # Upwelling anomalies spring
BenNear <- merge(BenNear,WTmp_mn_ann_hobo,all.x=T)# Annual mean water temp from HOBO loggers 
BenNear <- merge(BenNear,WaterTmp_Ann,all.x=T)    # Annual mean water temp from buoys
BenNear <- merge(BenNear,WaterTmp_Winter,all.x=T) # Winter mean water temp from buoys
BenNear <- merge(BenNear,WaterTmp_WAnom,all.x=T)  # Water Temp Anomoly Winter Mean from buoys
BenNear <- merge(BenNear,WaterTmp_SAnom,all.x=T)  # Water Temp Anomoly Spring Mean frp, buoys
#BenNear <- merge(BenNear,Waves_Ann,all.x=T)       # Annual mean significant wave hgt and period from buoys
#BenNear <- merge(BenNear,Waves_Winter,all.x=T)    # Winter mean significant wave hgt and period from buoys
#BenNear <- merge(BenNear,Wind_Ann,all.x=T)        # Annual mean wind speed and direction from buoys
#BenNear <- merge(BenNear,Wind_Winter,all.x=T)     # Winter mean wind speed and direction from buoys
BenNear <- merge(BenNear,Phy_spr,all.x=T)         # Phytoplankton - Seward Line, spring
BenNear <- merge(BenNear,Phy_fal,all.x=T)         # Phytoplankton - Seward Line, fall
BenNear <- merge(BenNear,Phy_yr,all.x=T)          # Phytoplankton - Seward Line, annual
#BenNear <- merge(BenNear,SatChl_df,all.x=T)       # Chla - Satellite annual
#BenNear <- merge(BenNear,SST,all.x=T)             # SST - Seward Line
BenNear <- merge(BenNear,Wlk_GOA,all.x=T)         # Whelks (Nucella sp.) abundance (n/m2)
#BenNear <- merge(BenNear,SS_GOA,all.x=T)          # Sea Stars abundance
BenNear <- merge(BenNear,BS_IA,all.x=T)           # Bare Substrate percent cover
BenNear <- merge(BenNear,b_IA,all.x=T)            # Barnacles percent cover
BenNear <- merge(BenNear,ms_IA,all.x=T)           # Mussels percent cover
BenNear <- merge(BenNear,Fd_IA,all.x=T)           # Fucus percent cover
BenNear <- merge(BenNear,Am_IA,all.x=T)           # Alaria percent cover
BenNear <- merge(BenNear,No_IA,all.x=T)           # Odonthalia / Neorhodomela sp. percent cover
BenNear <- merge(BenNear,BAann_IA,all.x=T)        # Brown Algae Annual percent cover
BenNear <- merge(BenNear,GAann_IA,all.x=T)        # Green Algae Annual percent cover
BenNear <- merge(BenNear,RAann_IA,all.x=T)        # Red Algae Annual percent cover
BenNear <- merge(BenNear,RAper_IA,all.x=T)        # Red Algae Perennial percent cover
BenNear <- merge(BenNear,RAall_IA,all.x=T)        # Red Algae Total percent cover
#BenNear <- merge(BenNear,ElG,all.x=T)             # Eelgrass percent cover
BenNear <- merge(BenNear,OyC_GOA,all.x=T)         # BLOY breeding adults abundance
#BenNear <- merge(BenNear,,all.x=T)      # BLOY diet prey size
#BenNear <- merge(BenNear,,all.x=T)      # BLOY diet prey abundance
#BenNear <- merge(BenNear,,all.x=T)      # BLOY diet proportion biomass
#BenNear <- merge(BenNear,BLOYAbun,all.x=T)        # BLOY abundance
#BenNear <- merge(BenNear,HADUAbun,all.x=T)        # HADU abundance
#BenNear <- merge(BenNear,BAGOAbun,all.x=T)        # BAGO abundance
#BenNear <- merge(BenNear,Leuk_Abun_Size,all.x=T)  # Leukoma abundance, mean size, mean bimoass
#BenNear <- merge(BenNear,Maco_Abun_Size,all.x=T)  # Macoma abundance, mean size, mean biomass
#BenNear <- merge(BenNear,Saxi_Abun_Size,all.x=T)  # Saxidomus abundance, mean size, mean biomass
#BenNear <- merge(BenNear,SOf_Crab_Bmss,all.x=T)   # Sea Otter forage crab biomass
#BenNear <- merge(BenNear,SOf_Clam_Bmss,all.x=T)   # Sea Otter forage clam biomass
#BenNear <- merge(BenNear,SOf_Urch_Bmss,all.x=T)   # Sea Otter forage urchin biomass
#BenNear <- merge(BenNear,SOf_Muss_Bmss,all.x=T)   # Sea Otter forage mussel biomass
#BenNear <- merge(BenNear,SOf_Star_Bmss,all.x=T)   # Sea Otter forage sea star biomass
#BenNear <- merge(BenNear,SOf_Snail_Bmss,all.x=T)  # Sea Otter forage snail biomass
#BenNear <- merge(BenNear,SOf_Chi_Bmss,all.x=T)    # Sea Otter forage chiton biomass
#BenNear <- merge(BenNear,SOf_Octo_Bmss,all.x=T)   # Sea Otter forage octopus biomass
#BenNear <- merge(BenNear,SOf_Worm_Bmss,all.x=T)   # Sea Otter forage worm biomass
#BenNear <- merge(BenNear,SOf_Other_Bmss,all.x=T)  # Sea Otter forage "other" biomass
#BenNear <- merge(BenNear,SOAD,all.x=T)            # Sea Otter mean carcass tooth age; proportion of prime age
#BenNear <- merge(BenNear,SOPop,all.x=T)           # Sea Otter population estimates
BenNear <- merge(BenNear,REG,all.x=T)             # Sea Otter energy recovery (kcal/min)
BenNear <- merge(BenNear,FWDisc_Spring,all.x=T)   # Freshwater Discharge Spring Mean
BenNear <- merge(BenNear,FWDisc_Fall,all.x=T)     # Freshwater Discharge Fall Mean
BenNear <- merge(BenNear,FWDisc_Yearly,all.x=T)   # Freshwater Discharge Annual Mean


BenNear <- arrange(BenNear,Region,Site_Name,Year,Quadrat)

# head(BenNear)

# Optional: Write data frame to a CSV
#write.csv(BenNear, file = "BenthicNearshore_MusselQuestData.csv", row.names=FALSE)



##############################################################################################









