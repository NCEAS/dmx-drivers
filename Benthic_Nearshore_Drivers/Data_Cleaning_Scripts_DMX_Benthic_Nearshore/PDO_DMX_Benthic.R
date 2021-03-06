###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
#####  Pacific Decadal Oscillation Index (PDO): 
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
###  Pacific Decadal Oscillation Index (PDO): 

URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"
pdo_raw <- read_html(URL_pdo)
pdo_pre <- pdo_raw %>% 
           html_node("p") %>%
           html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=31, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=32, nrows=118, stringsAsFactors=F, sep="", 
                     header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4)  # removes asterisks from years 2002-2015
#
pdo_ann <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
      #    filter(Year %in% c(2010:2015)) %>% # selects years 
           gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           group_by(Year) %>%
           summarise(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           ungroup() 

# Spring PDO March through May 
pdo_spr <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
      #    filter(Year %in% c(2010:2015)) %>% # selects years 
           gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           filter(Month %in% c("MAR","APR","MAY")) %>%
           group_by(Year) %>%
           summarise(PDO_spring_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           ungroup() 

# Winter PDO Dec through Feb
pdo_win <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
      #    filter(Year %in% c(2010:2015)) %>% # selects years 
           gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           filter(Month %in% c("JAN","FEB","DEC")) %>%
           group_by(Year) %>%
           summarise(PDO_winter_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           ungroup() 


PDO_ann_spr <- merge(pdo_ann,pdo_spr,all.x=T)
PDO_ann_seasn <- merge(PDO_ann_spr,pdo_win,all.x=T)  
  
  
  
  
  
  
  