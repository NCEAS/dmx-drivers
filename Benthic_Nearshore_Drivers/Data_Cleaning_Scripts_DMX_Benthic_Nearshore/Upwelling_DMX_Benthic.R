###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Pacific Upwelling Anomalies: 
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
### Upwelling Anomalies: 
# MONTHLY UPWELLING ANOMALIES (units are cubic meters/second/100 meters of coastline)
URL_upanom <- "http://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upanoms.mon"
upanom_raw <- read_html(URL_upanom)
upanom_pre <- upanom_raw %>% 
              html_node("p") %>%
              html_text()
upanom_cols <- scan(textConnection(upanom_pre), skip=2, nlines=1, what=character())# Get header row
upanom_cols <- c("Lat", "Long", upanom_cols[-1])# split position into lat and long 
upanom_df <- read.csv(file=textConnection(upanom_pre), skip=4, stringsAsFactors=F, sep="", 
                   header=FALSE, col.names=upanom_cols, strip.white=TRUE)


upanom_df1 <- upanom_df %>%
              filter(Long %in% c("146W","149W")) %>%  # subsets for the two sites in the GOA
              dplyr::rename(Year=YEAR) %>% # rename data columns
              gather(Month, UpwelAnom,-Year,-Lat,-Long)  # reshapes data to be column-wise

###
upanom_spr <- upanom_df1 %>% 
              filter(Month %in% c("MAR", "APR", "MAY")) %>%
              group_by(Year) %>%
              summarise(UpWelAnom_spr_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
              ungroup() 

upanom_fal <- upanom_df1 %>%
              filter(Month %in% c("SEP", "OCT", "NOV")) %>%
              group_by(Year) %>%
              summarise(UpWelAnom_fal_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
              ungroup() 

upanom_ann <- upanom_df1 %>%
              group_by(Year) %>%
              summarise(UpWelAnom_anul_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
              ungroup() 



