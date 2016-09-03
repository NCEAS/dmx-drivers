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
colnames(Chl_df)[10] <- "chlorophyllA"
#Check it
head(Chl_df) ; str(Chl_df)



##
# Function to format the date stored as character into the date format
date_formatter <- function(df, date_column="Date") {
                  df_tmp <- df
                  # check if there is a date column
                  if(!("Date" %in% colnames(df_tmp))){
                     date_column = "FakeDate"
                     if(df_tmp[["Season"]][[1]] %in% c("Spring")){
                        df_tmp[[date_column]] <- paste("05/01/",df_tmp[["Year"]], sep="")     
                  } else if(df_tmp[["Season"]][[1]] %in% c("Fall")){
                            df_tmp[[date_column]] <- paste("09/01/",df_tmp[["Year"]], sep="")
                  }}
                  
                  df_tmp[["Datef"]] <- as.Date(df_tmp[[date_column]], format = "%m/%d/%Y")
                  # Handle the case the date format is already formatted as dd-Mon-yy
                  if (sum(is.na(df_tmp[["Datef"]])) == length(df_tmp[[date_column]])) {
                         df_tmp[["Datef"]] <- as.Date(df_tmp[[date_column]],"%d-%b-%y")
                     }
                 
                  return(df_tmp)
                  }
##

# Get more years of data 2011 - 2015
load_my_data <- function(path) { 
                # read the other csv
                files <- dir(path, pattern = '\\.csv', full.names = TRUE)
                tables <- lapply(files, function(x) read.csv(x, skip=3, fileEncoding="latin1", 
                                                             stringsAsFactors = FALSE))
                # add formatted date column
                tables <- lapply(tables, date_formatter)
                # Merge all the other csv into one dataframe
                #one_df <- merge_recurse(tables)
         
                return(tables)
                }

list_csvs <- load_my_data("./Seward_Line_Chla")

# Clean column names, etc.in the csvs
colnames(list_csvs[[1]])[3] <- "chlorophyllA" 
colnames(list_csvs[[1]])[4] <- "phaeophytin" 
colnames(list_csvs[[2]])[3] <- "chlorophyllA" 
colnames(list_csvs[[2]])[4] <- "phaeophytin" 

list_csvs[[3]] <- dplyr::select(list_csvs[[3]], -c(X, X.1))
colnames(list_csvs[[3]])[6] <- "chlorophyllA"

list_csvs[[4]] <- dplyr::select(list_csvs[[4]], -X)
colnames(list_csvs[[4]])[7] <- "chlorophyllA"

colnames(list_csvs[[5]])[4] <- "chlorophyllA"
colnames(list_csvs[[5]])[5] <- "phaeophytin"

list_csvs[[6]] <- list_csvs[[6]][,c(1:7,11)]
  
  #dplyr::select(list_csvs[[6]], -c(X.20_Chl_A..µg.L..1, Total_Chl_A..µg.L.,
  #                                                 Total_Phaeo..µg.L.))
colnames(list_csvs[[6]])[7] <- "chlorophyllA"

colnames(list_csvs[[7]])[7] <- "chlorophyllA"
colnames(list_csvs[[8]])[7] <- "chlorophyllA"
colnames(list_csvs[[9]])[4] <- "chlorophyllA"
colnames(list_csvs[[9]])[5] <- "phaeophytin"
colnames(list_csvs[[10]])[7] <- "chlorophyllA"
colnames(list_csvs[[11]])[1] <- "CTD_Cast"
colnames(list_csvs[[11]])[2] <- "Station_Name"
colnames(list_csvs[[11]])[3] <- "lat"
colnames(list_csvs[[11]])[4] <- "lon"
colnames(list_csvs[[11]])[7] <- "Depth_m"
colnames(list_csvs[[11]])[9] <- "chlorophyllA"

# Merge all the other csvs into one dataframe
one_df <- merge_recurse(list_csvs)

# Merge with the main Chlorophyll data
Chl_ALL <- merge(Chl_df, one_df, all=T, 
                 by = c("Station_Name", "Datef", "Depth_m", "lat", "lon", 
                        "chlorophyllA", "phaeophytin"))

# DF of GAK1 sites
Chl_GAK1 <- Chl_ALL %>%
            mutate_each(funs(toupper), Station_Name) %>%  # capitalize station names
            filter(Station_Name %in% c("GAK 1", "GAK1", "GAK01")) %>%  # select GAK 1 only  #, "GAK01I", "GAK 1I", "GAK1I"
            arrange(Datef) %>%     
            mutate(Year=substring(Datef,1,4),
                   Month=substring(Datef,6,7)) %>% 
            filter(!(Size_Fraction %in% c(">20 µm", "<20 µm", "Total_Phaeo (µg/L)", ">20", "<20"))) 
          

#filter(str_detect(Treatment, "non"))

#################
### NOTE: Have Jessica correct the dates for 2007 (swapped Month and Day)
### in the data sheet on the portal.  
#################
#
Phy_spr <- Chl_GAK1 %>%
           filter(Month %in% c("04", "05")) %>%    # selects just the May samples for all years
           group_by(Year) %>%
           summarise(ChlA_micgL_SpMn=mean(as.numeric(chlorophyllA), na.rm=TRUE),
                     TotChl_micgL_SpMn=mean(totalChl)) %>% # get annual means
           ungroup() %>%
           mutate(TotChlA_micgL_SpMn=rowSums(.[2:3],na.rm=T)) %>%
           select(Year, TotChlA_micgL_SpMn)

Phy_fal <- Chl_GAK1 %>%
           filter(Month %in% c("09","10")) %>%    # selects just the Sept & Oct samples for all years
           group_by(Year) %>%
           summarise(ChlA_micgL_FlMn=mean(as.numeric(chlorophyllA), na.rm=TRUE),
                     TotChl_micgL_FlMn=mean(totalChl)) %>% # get annual means
           ungroup() %>%
           mutate(TotChlA_micgL_FlMn=rowSums(.[2:3],na.rm=T)) %>%
           select(Year,TotChlA_micgL_FlMn)

Phy_yr <- Chl_GAK1 %>%
          group_by(Year) %>%
          summarise(ChlA_micgL_AnnMn=mean(as.numeric(chlorophyllA), na.rm=TRUE),
                   TotChl_micgL_AnnMn=mean(totalChl)) %>% # get annual means
          ungroup() %>%
          mutate(TotChlA_micgL_AnnMn=rowSums(.[2:3],na.rm=T)) %>%
          select(Year,TotChlA_micgL_AnnMn)

# NOTE Units are ug/L

# Just use data from both seasons and annually and just from most inshore one, which GAK1

