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
             mutate(Year = as.numeric(sapply(strsplit(as.character(Decimal_Year), split="[.]") , 
                                  function(x) x[1])),
                    Month_dec = str_sub(Decimal_Year, 6,8),
                    Month_dec = paste0("0.", Month_dec),
                    Month = round(((Decimal_Year-Year)*12)+0.5)) %>%
             filter(Year > "1999")
                    
                    
Fresh_H2O_Spr <- Fresh_H2O %>%
                 filter(Month %in% c("4", "5")) %>%    # selects spring samples for all years
                 group_by(Year) %>%
                 summarise(SC_Mn_FWDisc_SpMn = mean(as.numeric(SC_Monthly_Mn_Disc_cubm), na.rm=TRUE),
                           Ttl_FWDisc_Anom_SpMn = mean(as.numeric(Total_Disc_Anom), na.rm=TRUE)) %>%
                 ungroup()


Fresh_H2O_Fal <- Fresh_H2O %>%
                 filter(Month %in% c("9", "10")) %>%    # selects fall samples for all years
                 group_by(Year) %>%
                 summarise(SC_Mn_FWDisc_FlMn = mean(as.numeric(SC_Monthly_Mn_Disc_cubm), na.rm=TRUE),
                           Ttl_FWDisc_Anom_FlMn = mean(as.numeric(Total_Disc_Anom), na.rm=TRUE)) %>%
                 ungroup()


Fresh_H2O_Yr <- Fresh_H2O %>%
                group_by(Year) %>%
                summarise(SC_Mn_FWDisc_AnMn = mean(as.numeric(SC_Monthly_Mn_Disc_cubm), na.rm=TRUE),
                          Ttl_FWDisc_Anom_AnMn = mean(as.numeric(Total_Disc_Anom), na.rm=TRUE)) %>%
                ungroup()

