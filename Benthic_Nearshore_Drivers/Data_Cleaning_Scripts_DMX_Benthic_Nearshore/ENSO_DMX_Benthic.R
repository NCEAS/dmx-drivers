###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Multivariate El Nino Southern Oscillation Index (MEI):
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
###  Multivariate ENSO Index (MEI): 

URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
enso_pre <- xpathSApply(htmlParse(content(GET(URL_enso))),"//html/body/pre", xmlValue)
enso_cols <- scan(textConnection(enso_pre), skip=10, nlines=1, what=character()) # get header row
enso <- read.csv(file=textConnection(enso_pre), skip=11, stringsAsFactors=F, sep="\t", 
                 header=FALSE, col.names=enso_cols)
enso_df <- enso[1:67,]  # removes the text at bottom of file

enso_df1 <- enso_df %>%
            dplyr::rename(Year=YEAR) %>% # rename data columns
            gather(Months, ENSO, -Year) %>% # reshapes data to be column-wise
            filter(!is.na(ENSO)) # remove NA values

###
ENSO_ann <- enso_df1 %>%
            group_by(Year) %>%
            summarise(ENSO_anul_mn=mean(ENSO)) %>% # get annual means
            ungroup()  # 

ENSO_spr <- enso_df1 %>%
            filter(Months %in% c("FEBMAR", "MARAPR", "APRMAY")) %>%
            group_by(Year) %>%
            summarise(ENSO_spr_mn=mean(ENSO)) %>% # get annual means
            ungroup() 

ENSO_fal <- enso_df1 %>%
            filter(Months %in% c("AUGSEP", "SEPOCT", "OCTNOV")) %>%
            group_by(Year) %>%
            summarise(ENSO_fal_mn=mean(ENSO)) %>% # get annual means
            ungroup() 



