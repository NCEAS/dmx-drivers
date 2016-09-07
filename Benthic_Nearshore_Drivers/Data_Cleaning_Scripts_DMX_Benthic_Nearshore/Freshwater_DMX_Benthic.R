###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Freshwater Discharge (GAK1): 
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
# GAK 1 Freshwater Discharge
URL_fd <- GET("http://www.ims.uaf.edu/gak1/data/FreshwaterDischarge/Discharge.dat")
fd1 <- content(URL_fd, as='text')
#fd_cols <- read.table(textConnection(fd1), nrows=8, fill=TRUE, stringsAsFactors=FALSE, header=FALSE)# Get header row
fd_col_nms <- c("Decimal_Year", "SE_Monthly_Mn_Disc_cubm", "SC_Monthly_Mn_Disc_cubm",
                "Total_Disc_Seward_SC_plus_SE", "Total_Disc_Anom") # column names
fd_df <- read.csv(file=textConnection(fd1), skip=7, fill=TRUE, col.names=fd_col_nms, 
                  stringsAsFactors=FALSE, header=FALSE)    

Fresh_H2O <- fd_df %>%
             mutate(Year = sapply(strsplit(as.character(Decimal_Year), split="[.]") , 
                                  function(x) x[1]),
                    Month = str_sub(Decimal_Year, 6,8),
                    Month = ((Decimal_Year−Year)*365)+0.5)


       




