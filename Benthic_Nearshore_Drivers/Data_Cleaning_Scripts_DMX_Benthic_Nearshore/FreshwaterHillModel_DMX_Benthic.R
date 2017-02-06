###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Hill Freshwater Discharge model data
###########################################################

# Model data is publically available here: 
# http://portal.aoos.org/gulf-of-alaska.php#module-metadata/2c11c0f6-be73-4044-8dd2-55d8b59bb203/eb620727-5858-4190-b614-c85c0a359ff1

# We downloaded the nearest points for our 15 sites of interest in the 3 regions. 
# That subset of data loads from the GitHub repo, since it was too cumbersome to download it
# directly in a script. 


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

# load data 
FW_file_locs <- list("./Freshwater_11-20-2016/VirtualSensorOutput_A/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_B/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_C1/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_C2/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_C3/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_D1/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_D2/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_E/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_F/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_G/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_H/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_I/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_J/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_K/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_L/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_M/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_N/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_O1/q.csv",
                     "./Freshwater_11-20-2016/VirtualSensorOutput_O2/q.csv")

# file codes 
FW_file_codes <- read.csv("./Freshwater_11-20-2016/sites_latlong.csv")

# function that will add a column to each file with the site name, and
# subset the years to ones that we are using (2005:2015)

#' Data Cleaning Function
#' @param df_location 
#' @return FWA1
#' @export
#' @examples
FW_Model_Data <- function(df_location){
                 # read in the df
                 FWA <- read.csv(df_location, stringsAsFactors = FALSE)
                 # clean up the df
                 FWA1 <- FWA %>%
                         dplyr::rename(FW_Discharge_ft3_s1 = 
                                       http...mmisw.org.ont.ioos.parameter.river_discharge..ft3.s.1.,
                                       Latitude = latitude..degree.,
                                       Longitude = longitude..degree.) %>%
                         select(-station_id, -sensor_id) %>%
                         mutate(File_Code = str_sub(df_location, 45,46),
                                Year = str_sub(FWA$date_time, 1,4),
                                Month = str_sub(FWA$date_time, 6,7),
                                Day = str_sub(FWA$date_time, 9,10)) %>%
                         filter(Year %in% c(2005:2015))
                 return(FWA1)
                 }


FWDisc_df_list <- lapply(FW_file_locs, FUN=FW_Model_Data) # for every element of the list of file locs run my function

FWDisc_a1 <- bind_rows(FWDisc_df_list) # bind the list of dataframes output by lapply() into one large dataframe

# add in the site and region columns
FWDisc_all <- FWDisc_a1 %>%
              mutate(Site = ifelse(File_Code == "A/", "Amalik Bay",
                            ifelse(File_Code == "B/", "Kaflia Bay",
                            ifelse(File_Code %in% c("C1","C2","C3"), "Kinak Bay",
                            ifelse(File_Code %in% c("D1", "D2"), "Kukak Bay",
                            ifelse(File_Code == "E/", "Takli Island",
                            ifelse(File_Code == "F/", "Aialik Bay",
                            ifelse(File_Code == "G/", "Harris Bay",
                            ifelse(File_Code == "H/", "McCarty Fjord",
                            ifelse(File_Code == "I/", "Nuka Bay",
                            ifelse(File_Code == "J/", "Nuka Passage", 
                            ifelse(File_Code == "K/", "Herring Bay",
                            ifelse(File_Code == "L/", "Hogan Bay",
                            ifelse(File_Code == "M/", "Iktua Bay",
                            ifelse(File_Code == "N/", "Johnson Bay",
                            ifelse(File_Code %in% c("O1", "O2"), "Whale Bay", 
                                   ""))))))))))))))),
                     Region = ifelse(Site %in% c("Amalik Bay","Kaflia Bay","Kinak Bay",
                                                 "Kukak Bay","Takli Island"), "KATM",
                              ifelse(Site %in% c("Aialik Bay","Harris Bay","McCarty Fjord",
                                                 "Nuka Bay","Nuka Passage"), "KEFJ",
                              ifelse(Site %in% c("Herring Bay","Hogan Bay","Iktua Bay",
                                                 "Johnson Bay","Whale Bay"), "WPWS", "")))) %>%
              arrange(Region, Site, Year, Month, Day)
              

# FWDisc_Monthly <- FWDisc_all %>%
#                   select(-date_time) %>%
#                   group_by(Region, Site, Latitude, Longitude, File_Code, Year, Month) %>%
#                   summarize(FWDisc_MeanMonthly_ft3s1 = mean(FW_Discharge_ft3_s1)) %>%
#                   ungroup()

FWDisc_Spring <- FWDisc_all %>%
                 select(-date_time) %>%
                 filter(Month %in% c("03", "04", "05")) %>%
                 group_by(Region, Site, Latitude, Longitude, File_Code, Year) %>%
                 summarize(FWDisc_MeanSpring_ft3s1 = mean(FW_Discharge_ft3_s1)) %>%
                 ungroup() %>% 
                 select(Region, Site, Year, FWDisc_MeanSpring_ft3s1)


FWDisc_Fall <- FWDisc_all %>%
               select(-date_time) %>%
               filter(Month %in% c("03", "10", "11")) %>%
               group_by(Region, Site, Latitude, Longitude, File_Code, Year) %>%
               summarize(FWDisc_MeanFall_ft3s1 = mean(FW_Discharge_ft3_s1)) %>%
               ungroup() %>% 
               select(Region, Site, Year, FWDisc_MeanFall_ft3s1)


FWDisc_Yearly <- FWDisc_all %>%
                 select(-date_time) %>%
                 group_by(Region, Site, Latitude, Longitude, File_Code, Year) %>%
                 summarize(FWDisc_MeanYearly_ft3s1 = mean(FW_Discharge_ft3_s1)) %>%
                 ungroup() %>% 
                 select(Region, Site, Year, FWDisc_MeanYearly_ft3s1)


  
  
  
  
  
  
  
  
  
