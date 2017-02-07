###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Hobo Logger Temperature Data: pre-processed by Dan Monson in SASS
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

URL_HWT <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-ueXVIUE02cS1HeFE"
HWTGet <- GET(URL_HWT)
HWT1 <- content(HWTGet, as='text')
HWT <- read.table(file=textConnection(HWT1), sep="\t", header=TRUE)
head(HWT)


HoboWTmp <- HWT %>%
            dplyr::rename(Region=block, Year=year) %>%
            separate(col=sensor, into=c("Site_Name", "Elevation"), sep="0") %>%
            select(-Elevation) %>%
            mutate(Site_Name = ifelse(Site_Name=="Kukak_",'Kukak Bay',
                               ifelse(Site_Name=="Aialik_",'Aialik Bay',       
                               ifelse(Site_Name=="Hogan_",'Hogan Bay',   
                               ifelse(Site_Name=="Kaflia_",'Kaflia Bay',
                               ifelse(Site_Name=="McCarty_",'McCarty Fjord',
                               ifelse(Site_Name=="Iktua_",'Iktua Bay',  
                               ifelse(Site_Name=="Kinak_",'Kinak Bay',  
                               ifelse(Site_Name=="Nuka_Bay_",'Nuka Bay',  
                               ifelse(Site_Name=="Whale_",'Whale Bay',
                               ifelse(Site_Name=="Amalik_",'Amalik Bay',  
                               ifelse(Site_Name=="Nuka_Pass_",'Nuka Passage',       
                               ifelse(Site_Name=="Johnson_",'Johnson Bay',    
                               ifelse(Site_Name=="Takli_",'Takli Island',  
                               ifelse(Site_Name=="Harris_",'Harris Bay', 
                               ifelse(Site_Name=="Herring_",'Herring Bay',   
                               "")))))))))))))))
                   ) 



Hobo_WTmp_ann <- HoboWTmp %>%
                 select(-X_TYPE_, -X_FREQ_, -std_w_temp) %>%
                 group_by(Region, Site_Name, Year) %>%
                 summarise(Hobo_WaterTemp_AnnMn=mean(mean_w_temp),
                           Hobo_WaterTemp_AnnMin=min(min_w_temp),
                           Hobo_WaterTemp_AnnMax=max(max_w_temp)) %>%
                 ungroup() %>%
                 arrange(Site_Name, Year)


# # Examine data for holes in the data
# melt_WTmp <- reshape::melt.data.frame(as.data.frame(Hobo_WTmp_ann), id.vars=c("Year","Region","Site_Name"),      
#                       variable_name="WTmp_type")
# melt_WTmp <- arrange(melt_WTmp, Site_Name, Year)
# 
# # 15 sites, 3 data types, 10 years = 450 rows that the full dataframe should have
# 
# all_yrs <- data.frame(Year = rep(c(2006,2006,2006,2007,2007,2007,2008,2008,2008,2009,2009,2009,
#                                    2010,2010,2010,2011,2011,2011,2012,2012,2012,2013,2013,2013,
#                                    2014,2014,2014,2015,2015,2015), 15),
#                       Region = rep(c("WPWS","KEFJ","KATM"), 15))
# 
# all_info <- all_yrs[rep(seq_len(nrow(all_yrs)), each=5),]


WTmp_mn_ann_hobo <- Hobo_WTmp_ann %>% #all_info %>%
                   # full_join(Hobo_WTmp_ann) %>%
                    select(-Hobo_WaterTemp_AnnMin, -Hobo_WaterTemp_AnnMax)# %>%
                    # remove any row with a NA in it, since in this case it means it wasn't sampled
                   # filter(complete.cases(.))


























