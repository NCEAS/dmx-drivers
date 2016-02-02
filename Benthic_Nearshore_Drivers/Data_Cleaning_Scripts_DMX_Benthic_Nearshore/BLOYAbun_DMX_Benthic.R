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
# Black Oyster Catcher Abundance


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

conn <- odbcConnectAccess2007(path.expand("./NPPSD_Back_v2.mdb")) # establish a connection
DATA_OBS <- sqlFetch(conn,"tbl_DATA_OBS")  # read in a table
LOCATION <- sqlFetch(conn,"tbl_LOCATION") 

close(conn) 
unlink(NPPSDzipd)


# Filter and Sort data       # Density is Number of Birds per km2
EPWS <- c("60.63751-145.99677", "60.67889-145.88132" ,"60.60282-145.7447", "60.81954-146.56397", 
          "60.45094-146.25966", "61.13059-146.57246", "60.5051-146.06936"  ,"60.72072-146.62345", 
          "60.86553-146.72156", "60.75748-146.17368" ,"60.6914-146.40106",  "60.34233-146.67664",
          "60.682-146.1952"    ,"60.74481-146.12938", "60.89067-146.22897", "60.70833-146.18333" ,
          "60.51891-146.21928", "60.87299-146.69811", "60.60974-146.0005"  ,"61.12291-146.5931", 
          "60.71885-146.3548" , "61.11724-146.5953"  ,"60.74438-146.7013",  "61.08296-146.4702")
          
NPWS <- c("60.68358-148.63096", "61.01684-148.33919", "60.87977-147.42312", "60.97346-147.0256" , 
          "61.02247-147.53496", "61.06636-148.3209" , "60.61678-148.13882", "61.07723-147.99703",
          "60.8084-147.6658"  , "61.09167-148.18333", "60.92156-147.59944", "61.03009-148.03037",
          "61.14802-147.90549", "60.79667-147.75582", "61.02461-148.09689","60.70486-148.65478",
          "60.6864-147.84429",  "60.98396-146.97757", "60.82073-147.71566", "60.84006-147.63052", 
          "60.99393-147.01785", "60.84583-147.43333", "60.72493-147.88347", "60.83945-147.73674",
          "61.21667-147.7667",  "60.97348-148.4246",  "60.97858-148.4329" , "61.05631-148.3322", 
          "61.06645-148.2945",  "60.96562-148.0275" , "60.89124-147.6031",  "61.17756-147.8643",  
          "60.7374-147.902"   ,"60.78263-147.7216",  "60.84984-147.9143",  "60.80474-147.6786" ,
          "60.85968-147.8004",  "61.01519-147.5548",  "60.99128-147.0201")

WPWS <- c("60.36525-147.84593", "60.41079-148.0922",  "60.38808-147.63861", "60.31161-147.9221",  
          "60.39567-147.6483" , "60.11354-148.02782", "60.21986-148.20374", "60.27073-148.27942",
          "60.08222-148.10955", "59.91988-147.80313", "60.39529-148.0013" , "60.12901-148.18227",
          "60.23333-148.31667", "60.25548-147.3977",  "60.27285-147.18545", "60.68779-147.43017", 
          "60.65638-147.43858", "60.23539-147.79214", "60.01191-148.01167", "60.03902-147.91486", 
          "60.19514-147.534"  , "60.29036-147.33931", "60.15721-147.37572", "60.24835-147.19614",
          "60.34186-147.05664", "60.43774-147.93204", "60.41686-147.83279", "60.07886-148.36669",
          "60.11627-148.26313", "60.26042-148.26667", "60.35759-148.2284",  "60.37921-148.13669", 
          "60.63274-147.6573" , "60.50889-147.70528", "60.20108-147.3133",  "60.17091-148.03757",
          "60.16565-148.00233", "59.94475-148.18793", "60.19001-148.11122", "60.41333-147.76291",
          "60.38972-147.81645", "60.2877-147.90012" , "60.42726-148.01197", "59.93429-148.17525", 
          "60.44185-147.73366", "60.39852-147.9936",  "60.23444-148.2369",  "60.0598-147.8992"  ,
          "59.9427-148.1236" ,  "59.9236-147.8151"  , "60.22384-147.4937" , "60.1252-147.4048" , 
          "60.61667-147.3761",  "60.7188-147.3634"  , "60.25224-147.2329",  "60.50025-147.0923",
          "59.94551-148.1198" , "59.89736-147.8165" , "60.10364-148.2913",  "60.159-148.093"  ,
          "60.27655-147.3375",  "60.30436-147.3368",  "60.12735-147.4095" , "60.18542-147.3295")

#zzzz <- readClipboard()

BLOYAbun <- DATA_OBS %>%
            filter(`NPPSD 4-Letter Code`=="BLOY", `PI Credit`=="David Irons") %>%
            merge(LOCATION, by=c("Master Key","Sample Area","Source","PI Credit")) %>%
            filter(!(`Modified Behavior` %in% c("Land","Boat","Dead")),
                   !(Density=="NA")) %>%
            mutate(LatLon = paste(Lat,Lon,sep=""),
                   Region = ifelse((LatLon %in% EPWS),'EPWS',
                            ifelse((LatLon %in% NPWS),'NPWS',
                            ifelse((LatLon %in% WPWS),'WPWS',"")))) %>%
            group_by(Region,Year) %>%
            summarise(BLOY_MnDensity_km2=mean(Density)) %>% # get annual regional means
            ungroup() %>%
            select(BLOY_MnDensity_km2,Year,Region)


# NOTE: data for years 1989 1990 1991 1993 1994 1996 1998 2000
# No recent data from David Irons in this dataset.  Hmmm....
                
# filter(Year %in% c(2010,2011,2012,2013,2014,2015)) %>%       




