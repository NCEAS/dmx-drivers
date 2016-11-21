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
BenNearMuss <- read.csv("../BenthicNearshore_MusselQuestData.csv")




