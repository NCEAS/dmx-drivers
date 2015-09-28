##############################################################
###  Script for cleaning RACE Groundfish data (from NOAA)  ###
###  This data is archived on the AOOS GoA Portal          ###
###  http://gulfwatch.nceas.ucsb.edu/#view/df35b.258.8     ###
###  Script created 28 Aug 2015                            ###
##############################################################

# NOTE: NOAA RACE groundfish data is the same as the NOAA large mesh trawl data

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)

## create empty data frame with Year column
Gf_RACE <- data.frame('Year'=c(1984,1987,1990,1993,1996,1999,2001,2003,2005,2007,2009,2011,2013))

## Steps for adding data columns: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)
## 3) merge with CoPlc dataframe:   CoPlc=merge(CoPlc,newData,all.x=T)  # rename new df CoPlc

# Reading in the data: currently from 1984 - 2011 
URL_Gfsh <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.257.2"
GfshGet <- GET(URL_Gfsh)
Gfsh1 <- content(GfshGet, as='text')
Gfsh <- read.csv(file=textConnection(Gfsh1),stringsAsFactors=F)
head(Gfsh)

# Assign Zero values to hauls with no catch
summary(as.factor(Gfsh$HAUL)) # this looks at the number of rows for each Haul 
# Doesn't appear to be any hauls with no catch !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Read in the spreadsheet with Jim's functional groupings
SpGp <- read.xls('eweData/goaEwe/RACE_GoA_NameTranslator.xlsx',sheet='RACE_GoA_NameTranslator',pattern='SID',na.strings=c('',' '))

gfSpG=merge(Gfsh,SpGp,by='SID') # ,all.x=T w/o this argument, table is shortened so the DB must have spp that are not on Jim's group list. confirm Jim that we should omit these



# Aggregate by functional groupings
Gfsh <- Gfsh %>%
            


# What Jim R. did: 
# 1 aggregated functional groups, 
# 2 assign 0 values to sampled hauls with no catch, and 
# 3 some preliminary filtering of data to the selected spatial domain.


## Questions for Jim:
  # what should species be omitted if they are not on the RACE_GOA_NameTranslator.xlsx list?

