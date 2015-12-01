###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Harlequin Duck Abundance from North Pacific Pelagic Seabird Database: 
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
library(Hmisc)
library(RODBC)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############
# Harlequin Duck Abundance

NPPSDzipd <- tempfile()
download.file("http://alaska.usgs.gov/data/nppsd/nppsd.zip", NPPSDzipd, mode="wb")
NPPSDzip_L <- unzip(NPPSDzipd, list=TRUE)

NPPSDzip_DB <- NPPSDzip_L[grep(".mdb", NPPSDzip_L$Name),]$Name   # subsets the data files

unzip_read <- function(file_list){
              # for every file in zipped file list, do the following 
              z <- unzip(NPPSDzipd, file_list)
              rbind.fill(lapply(z, mdb.get))
              }

NPPSD_HADU <- unzip_read(NPPSDzip_DB)  ;  unlink(NPPSDzipd)

contents(NPPSD_HADU)



z <- unzip(NPPSDzipd, NPPSDzip_DB)
pzM<-mdb.get('z', tables='NPPSD_Back_v2')



#pzM<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='menutblTaxa') # species list
#pzCr<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='tblCruiseData') # cruise data
#pzTow<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='tblTowData') # Tow data, contains species concetrations per tow and year embeded in the TowID other metadata tables not needed








DAT <- read.csv('tbl_DATA_OBS.csv',header=T) ##  Observations
head(DAT) ; str(DAT)

STE <- read.csv('tbl_LOCATION.csv',header=T) ## Sample info
head(STE) ; str(STE)

SOT <- filter(DAT, Common.Name =="Sea Otter")   # select otter observations

SOTT <- SOT %>%
        merge(STE, by=c("Master.Key"))  %>%   # add in the sample info
        rename(Sample_Year=Year) %>%         # rename column
        filter(Sample_Year %in% c(2010,2011,2012,2013,2014))   #  select years of interest
head(SOTT)
