###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Wind data from buoys from National Buoy Data Center
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
# Wind data (annual): from National Buoy Data Center

# selected buoys:
# Station 46060 - WEST ORCA BAY  
      # near PWS sites
# Station 46077 - SHELIKOF STRAIT, AK
      # near KATM sites
# Station 46076 - CAPE CLEARE
      # near KEFJ sites
#############
#############
if(!exists("BuoyData")|!exists("Buoys_all")) {
# Function to read in all these annual bouy data text files, and make data frames of them.
BuoyData <- function(data_url){
            dataGet <- GET(data_url)
            data1 <- content(dataGet, as='text')

            # need to say get year from URL
            year <- as.numeric(str_sub(data_url,60,63))
            # get buoy number from URL
            buoynum <- str_sub(data_url,54,58)

            # need to say if year < 1999 do this...
            if (year < 1999) {
                              df <- read.table(file=textConnection(data1),fill=TRUE,
                                               stringsAsFactors=FALSE,header=TRUE)
                              df <- rename(df, YYYY=YY)
                              df$YYYY <- as.integer(paste(rep(19),df$YYYY,sep=""))
                              df$BuoyID <- rep(buoynum,nrow(df))
            # and if year is 1999 to 2006 do this...                  
            } else if (year %in% c(1999:2006)) {
                                                df <- read.table(file=textConnection(data1),fill=TRUE,
                                                                 stringsAsFactors=FALSE,header=TRUE)
                                                df$BuoyID <- rep(buoynum,nrow(df))
            # and if year >/= 2007 do this...
            } else {
                    data_h <- scan(textConnection(data1), nlines=1, what=character())  # reads first header line
                    data_h <- gsub("#YY", "YYYY", data_h)  # gets rid of column name with # in it
                    df <- read.table(file=textConnection(data1),fill=TRUE,
                                     stringsAsFactors=FALSE,skip=2,header=FALSE)
                    names(df) <- data_h   # pastes the header line in
                    df$BuoyID <- rep(buoynum,nrow(df))
                    df <- rename(df, WD=WDIR, BAR=PRES)
                    }

            return(df)
            }

#############

# List of URLs from which to pull the text files
B_URLs <- list("http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h1995.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h1996.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h1997.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h1998.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h1999.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2000.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2001.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2002.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2003.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2004.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2013.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2014.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46060h2015.txt.gz&dir=data/historical/stdmet/",
               #
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2013.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46077h2014.txt.gz&dir=data/historical/stdmet/",
               #
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2013.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2014.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2015.txt.gz&dir=data/historical/stdmet/"
               )

Buoy_df_list <- lapply(B_URLs, FUN=BuoyData) # for every element of the list of URLs run my function

Buoys_all <- bind_rows(Buoy_df_list) # bind the list of dataframes output by lapply() into one large dataframe
}

#
Wind_Ann <- Buoys_all %>%
            filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
            rename(Year = YYYY) %>%       # rename column for uniformity
            mutate(Region = ifelse((BuoyID =="46060"), "WPWS")) %>%  # add Region column
  
  

Wind_Winter <- Buoys_all %>%
               filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
               rename(Year=YYYY) #%>%       # rename column for uniformity
  

