###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Phytoplankton from Seward Line (annual spring mean): 
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
### Phytoplankton (annual spring mean): (from Seward Line dataset)
# Get 1998-2010 data 
URL_Chl <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.41.3"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1),stringsAsFactors=FALSE)
head(Chl_df)
#################
### NTOE: Have Jessica correct the dates for 2007 (swapped Month and Day)
### in the data sheet on the portal.  
#################
#
Phy <- Chl_df %>%
       arrange(dateTime) %>%     
       mutate(Year=substring(dateTime,1,4),
              Month=substring(dateTime,6,7)) %>%  
       filter(Month %in% c("05")) %>%  # selects just the May samples for all years
       group_by(Year) %>%
       summarise(ChlA_micgL_AnnSpMn=mean(chloropyllA),
                 TotChl_micgL_AnnSpMn=mean(totalChl)) %>% # get annual means
       ungroup() %>%
       mutate(TotChlA_micgL_AnnSpMn=rowSums(.[2:3],na.rm=T)) %>%
       select(Year,TotChlA_micgL_AnnSpMn)

