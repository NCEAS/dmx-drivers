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
library(reshape)


## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############
### Phytoplankton (annual spring mean): (from Seward Line dataset)
# Get 1998-2010 data 
URL_Chl <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.41.3"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1), stringsAsFactors=FALSE)
Chl_df$Datef<-as.Date(Chl_df$dateTime) # Setting the Date column as date format
# Matching column Names for the merge
colnames(Chl_df)[4] <- "Station_Name" 
colnames(Chl_df)[9] <- "Depth_m"
#Check it
head(Chl_df) ; str(Chl_df)



##
# Function to format the date stored as character into the date format
date_formatter <- function(df, date_column="Date") {
                  df_tmp <- df
                  df_tmp[["Datef"]] <- as.Date(df[[date_column]], format = "%m/%d/%Y")
                  # Handle the case the date format is already formatted as dd-Mon-yy
                  if (sum(is.na(df_tmp[["Datef"]])) == length(df[[date_column]])) {
                      df_tmp[["Datef"]] <- as.Date(df[[date_column]],"%d-%b-%y")
                      }
                  return(df_tmp)
                  }
##


# Get more years of data 2011 - 2015
load_data <- function(path) { 
             files <- dir(path, pattern = '\\.csv', full.names = TRUE)
             tables <- lapply(files, function(x) read.csv(x, skip=3, fileEncoding="latin1", 
                                                          stringsAsFactors = FALSE))
             # add formatted date column
             tables <- lapply(tables, date_formatter)
             # Merge all the other csv into one dataframe
             one_df <- merge_recurse(tables)
         
             return(one_df)
             }

list_csvs <- load_data("./Seward_Line_Chla")




# Read the other csv
files <- dir(path, pattern = '\\.csv', full.names = TRUE)
tables <- lapply(files, function(x) read.csv(x, skip=3, fileEncoding="latin1",stringsAsFactors = FALSE))

# Add a formatted date
tables <- lapply(tables, date_formatter)

# Merge all the other csv into one dataframe
df_other <- merge_recurse(tables)

#Merge with the main Chlorophylle data
merge(Chl_df,df_other, by = c("Station_Name", "Datef", "Depth_m"), all =T)



#################
### NTOE: Have Jessica correct the dates for 2007 (swapped Month and Day)
### in the data sheet on the portal.  
#################
#
Phy <- Chl_df %>%
       arrange(dateTime) %>%     
       mutate(Year=substring(dateTime,1,4),
              Month=substring(dateTime,6,7)) %>%  
       filter(Month %in% c("05")) %>%    # selects just the May samples for all years
  #     filter(Year %in% c(2010:2015)) %>%  # selects years
       group_by(Year) %>%
       summarise(ChlA_micgL_AnnSpMn=mean(chloropyllA),
                 TotChl_micgL_AnnSpMn=mean(totalChl)) %>% # get annual means
       ungroup() %>%
       mutate(TotChlA_micgL_AnnSpMn=rowSums(.[2:3],na.rm=T)) %>%
       select(Year,TotChlA_micgL_AnnSpMn)



# Just use data from both seasons and annually and just from most inshore one, which GAK1

