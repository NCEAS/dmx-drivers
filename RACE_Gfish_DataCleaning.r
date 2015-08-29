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
URL_Gfsh <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.257.1"
GfshGet <- GET(URL_Gfsh)
Gfsh1 <- content(GfshGet, as='text')
Gfsh <- read.csv(file=textConnection(Gfsh1),stringsAsFactors=F)
head(Gfsh)

# Assign Zero values to hauls with no catch
summary(as.factor(Gfsh$HAUL)) # this looks at the number of rows for each Haul 
# Doesn't appear to be any hauls with no catch !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Read in the spreadsheet with Jim's functional groupings
setwd("C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/")   #######################
SpGp <- read.csv('RACE_GoA_NameTranslator.csv',header=T)    ####### NEED TO CHANGE THIS TO NOT READ IN FROM LOCAL MACHINE
head(SpGp)

# making lists of species IDs (SID column) in each functional grouping 
OMIT <- c(2, 3, 10002, 21725, 78005, 99990, 99991, 99992, 99993, 99994, 99998)  # These are species IDs (SID column)
misc_fish_shallow <- c(10, 21, 20000, 20001, 20002, 20004, 20005, 20006, 20007, 20015, 20018, 20022, 20034, 20035, 
                       20036, 20037, 20038, 20040, 20041, 20050, 20051, 20055, 20061, 20071, 20320, 20322, 20542, 
                       20700, 20702, 20712, 20713, 20720, 20842, 21705, 21710, 21731, 21735, 21752, 21770, 22010, 
                       22170, 22173, 22175, 22176, 22177, 22178, 22179, 22182, 22183, 22184, 22190, 22200, 22201, 
                       22203, 22204, 22205, 22206, 22208, 22210, 22214, 22215, 22216, 22219, 22220, 22226, 22228, 
                       22229, 22231, 22232, 22233, 22234, 22235, 22236, 22238, 22240, 22241, 22243, 22244, 22246, 
                       22249, 22250, 22251, 22255, 22258, 22262, 22265, 22270, 22271, 22274, 22276, 22284, 23791, 
                       23792, 24001)
dogfish                  
salmon_shark
sleeper_shark
other_skates             
fish_eggs
big_skate
longnose_skates          
misc_fish_deep
misc_flatfish
arrowtooth_flounder
P_Halibut
FH_sole
Dover_sole
Rex_sole
YF_Sole
AK_Plaice
S_rock_sole
N_rock_sole
sandlance                
sablefish
Bathylagidae
Myctophidae              
herring
other_managed_forage_fish_grenadiers
other_sculpins
large_sculpins
P_Cod
W_Pollock
greenlings
Atka_mackerel
P_hake
other_pelagic_smelt
eulachon                 
capelin
misc_salmonid
Chinook_salmon           
[40] coho salmon               pink salmon               chum salmon              
[43] sockeye salmon            eelpouts                  other Sebastes           
[46] shortspine thornyheads    rougheye rockfish         POP                      
[49] dusky rockfish            northern rockfish         sharpchin rockfish       
[52] shortraker rockfish       hydroids                  jellyfish unidentified   
[55] sea nettles               egg yolk jelly            Aequoria                 
[58] Aurelia                   lions mane jellyfish      corals                   
[61] sea pens                  anemones                  gelatinous filter feeders
[64] polychaetes               benthic amphipods         misc crustaceans         
[67] euphausiids               mysids                    NP shrimp                
[70] Pandalidae                misc crabs                Tanner crab              
[73] hermit crabs              King crab                 snails                   
[76] bivalves                  squids                    octopi                   
[79] sea stars                 urchins-dollars-cucumbers brittle stars            
[82] sponges                   misc worms                Urochordata   



# Aggregate by functional groupings
Gfsh <- Gfsh %>%
            


# What Jim R. did: 
# 1 aggregated functional groups, 
# 2 assign 0 values to sampled hauls with no catch, and 
# 3 some preliminary filtering of data to the selected spatial domain.




