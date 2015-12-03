###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Barrows Goldeneye Abundance from North Pacific Pelagic Seabird Database: 
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
# Barrows Goldeneye Abundance

NPPSDzipd <- tempfile()
download.file("http://alaska.usgs.gov/data/nppsd/nppsd.zip", NPPSDzipd, mode="wb")
NPPSDzip_L <- unzip(NPPSDzipd, list=TRUE)

NPPSDzip_DB <- NPPSDzip_L[grep(".mdb", NPPSDzip_L$Name),]$Name   # creates list of files I want
UNz <- unzip(NPPSDzipd, NPPSDzip_DB) # unzip the two mdb files (makes list I think)

mdb_table_list <- function(file_list){
                  # for every mdb file in the list, do the following 
                  conn <- odbcConnectAccess2007(path.expand(file_list)) # establish a connection
                  table_list <- subset(sqlTables(conn), TABLE_TYPE=="TABLE") # lists tables in DB
                  return(table_list)
                  }

lapply(UNz, mdb_table_list)  # running the function over the two .mdb files

DATA_OBS <- sqlFetch(conn,"tbl_DATA_OBS")  # read in a table
LOCATION <- sqlFetch(conn,"tbl_LOCATION") 
MasterKey <- sqlFetch(conn,"NPPSDv2_Masterkey_Crosswalk_3_19_14")

close(NPPSD_conn) 
unlink(NPPSDzipd)

# Filter and Sort data       # Density is Number of Birds per km2
BAGOAbun <- DATA_OBS %>%
            filter(`NPPSD 4-Letter Code`=="BAGO", `PI Credit`=="David Irons") %>%
            merge(LOCATION, by=c("Master Key","Sample Area","Source","PI Credit")) %>%
            filter(!(`Modified Behavior` %in% c("Land","Boat","Dead"))) %>%
            # Region info needs to be added here
            rename(BAGO_Density_km2=Density) %>%
            select(BAGO_Density_km2,Year)


# NOTE: data for years 1989 1990 1991 1993 1994 1996 1998 2000
# No recent data from David Irons in this dataset.  Hmmm....
                
# filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%       






