#####################################################
####  GoA Hydrocarbon Data Cleaning              ####
####   March 2015 ; Script by Rachael Blake      ####
#####################################################

# load packages
library(plyr) ; library(dplyr)


# Download the hydrocarbon database

HCDBzipd <- tempfile()
download.file("https://workspace.aoos.org/published/file/536d72c5e4b08354c761ae8c/2014_EVTHD_DataPackage.zip",
              HCDBzipd, mode="wb")
HCDBzip_L <- unzip(HCDBzipd, list=TRUE)
HCDBzip_DB <- HCDBzip_L[grep(".accdb", HCDBzip_L$Name),]$Name   # creates list of files I want
UNz <- unzip(HCDBzipd, HCDBzip_DB) # unzip the mdb files (makes list I think)

mdb_table_list <- function(file_list){
                  # for every mdb file in the list, do the following 
                  conn <- odbcConnectAccess2007(path.expand(file_list)) # establish a connection
                  table_list <- subset(sqlTables(conn), TABLE_TYPE=="TABLE") # lists tables in DB
                  return(table_list)
                  }
lapply(UNz, mdb_table_list)  # running the function over the two .accdb files

conn <- odbcConnectAccess2007(path.expand("./EVTHD 2014.accdb")) # establish a connection
ALKANE <- sqlFetch(conn,"Alkane")  # read in a table
PAH <- sqlFetch(conn,"PAH") 

close(conn) 
unlink(HCDBzipd)

#setwd("C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon Data")

#############################
# Calculate Total Aromatics  
PAH <- read.csv("PAH.csv")  # read in the PAH data file
head(PAH) ; str(PAH)

# Taking means of all chemical compound concentrations to get Total PAHs
PAH$TtlAromatic <- rowSums(PAH[,24:71], na.rm=T)   ; PAH[c(25:50),] # sum the chemicals across rows
PAH1 <- arrange(PAH, Sin)   # arrange by the Sample ID (Sin)
TotalAromat <- PAH1[PAH1$Sin > 0, -c(3,8,15:22,24:71)]   # remove rows with Sin < zero and individual compound columns


##########################
# Extract Total Alkanes
Alk <- read.csv("ALKANE.csv")  # read in the Alkanes data file
#library(plyr)  # only need to run this if you haven't loaded plyr previously
Alk <- dplyr::rename(Alk, QCbatch=QCBatch) # rename QCBatch column to match QCbatch column from PAH table
head(Alk) ; str(Alk)

# extracting Total Alkanes from the spreadsheet
TtlAlk <- Alk[,c(1,2,4:7,9:14,20,51)]   
#library(plyr)  # only need to run this if you haven't loaded plyr previously
TtlAlk1 <- arrange(TtlAlk, Sin)
TtlAlkane <- TtlAlk1[TtlAlk1$Sin > 0, ]   # remove rows with Sin < zero 


#########################
# Adding the data columns together
names(TotalAromat) ; names(TtlAlkane)  # make sure the columns are all named the same
#library(plyr)  # only run this if you haven't loaded plyr previously
AromAlk <-  join(TotalAromat, TtlAlkane, by="Sin", type="full")


#############################
# Adding in the Sample information 
SamIDs <- read.csv("sample.csv")
#library(plyr)
Samples1 <- arrange(SamIDs, Sin)  ; head(Samples1) # arranges the rows by Sample ID 
  

######################################
# Joining the two data frames together
TotalAromAlk1 <- join(AromAlk, Samples1, by="Sin", type="full")  # join data frames
TotalAromAlk <- arrange(TotalAromAlk1, Sin)  # sort by Sample ID (Sin)
head(TotalAromAlk) ; tail(TotalAromAlk)


###################################
# Cleaning and filtering the data
# remove the rows where QCERROR is "BIAS" and check the result using unique()
TotalAromAlk2 <- TotalAromAlk[!TotalAromAlk$QCERROR %in% "BIAS",]  ; unique(TotalAromAlk2$QCERROR)
# remove the rows where SampleType is "blank" and check the result using unique()
TotalAromAlk3 <- TotalAromAlk2[!TotalAromAlk2$SampleType %in% c("blank","BLANK","SPIKE"),] 
unique(TotalAromAlk3$SampleType)
# Remove rows with NAs in _BOTH_ the Aromatics and Alkanes columns
TotalAromAlk3a <- TotalAromAlk3[which(!is.na(TotalAromAlk3$TOTALKANES) | 
                                      !is.na(TotalAromAlk3$TtlAromatic)),]
# Remove rows with "BLANK", "QCSED" or "FBLANK" in the matrix column and check the result using unique()
TotalAromAlk3b <- TotalAromAlk3a[!TotalAromAlk3a$matrix %in% c("FBLANK","BLANK","QCSED"),]
unique(TotalAromAlk3b$matrix)
# Replace NAs in the column "Funding" with "EVOSTC" when the value in column "FundingSource" is "EVOSTC"
TotalAromAlk3b$Funding[is.na(TotalAromAlk3b$Funding) & TotalAromAlk3b$FundingSource=="EVOSTC"] <- "EVOSTC"
# Remove "FundingSource" column because it is redundant now
TotalAromAlk4 <- TotalAromAlk3b[,!names(TotalAromAlk3b) %in% c("FundingSource")]

### Remove NON-EVOSTC Samples (list confirmed by Mark Carls at NOAA Auk Bay Lab)
Non_EVOS <- read.csv("Non-EVOS SINs.csv") # read in the list of non_EVOS Sample ID numbers
head(Non_EVOS) ; nrow(Non_EVOS)

TotalAromAlk5 <- TotalAromAlk4[!TotalAromAlk4$Sin %in% Non_EVOS$Sin,]
head(TotalAromAlk5) ; nrow(TotalAromAlk5)
nrow(TotalAromAlk4) - nrow(TotalAromAlk5)  # should equal 440 even though there are 390 rows in
                                           # Non-EVOS because there are duplicates!!!

# Copy entries from "AnalysisType" column to "matrix" column only for rows with NA in matrix column
TotalAromAlk5$matrix <- as.character(TotalAromAlk5$matrix) # have to first make these columns character strings
TotalAromAlk5$AnalysisType <- as.character(TotalAromAlk5$AnalysisType)

TotalAromAlk5$matrix[is.na(TotalAromAlk5$matrix)] <- TotalAromAlk5$AnalysisType[is.na(TotalAromAlk5$matrix)] 

# unify the case of levels in the matrix column (ex: sediment and SEDIMENT)
TotalAromAlk5 <- TotalAromAlk5 %>%
                               mutate(matrix = tolower(matrix)) 

#########################
# create new CSV data file  (this should be changed to upload to repository later)
#write.csv(TotalAromAlk5, "C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon Data/Total_Aromatic_Alkanes_PWS.csv", row.names=F)









