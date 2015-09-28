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
dogfish <- c(210, 222, 310)                  
salmon_shark <- c(232)
sleeper_shark <- c(320)
other_skates <- c(400, 404, 405, 410, 435, 450, 455, 460, 471, 472, 475, 480, 483, 485, 495)             
fish_eggs <- c(1, 401, 402, 403, 421, 436, 441, 456, 461, 473, 474, 484, 711, 21422)
big_skate <- c(420)
longnose_skates <- c(440)          
misc_fish_deep <- c(710, 20110, 20120, 22300, 22310, 22320, 22390, 22701, 22702, 22703, 22704, 22951, 22952, 22955, 
                    23602, 23603, 23620, 23657, 23710, 23932, 23944, 23958, 23962)
misc_flatfish <- c(10001, 10010, 10020, 10112, 10115, 10129, 10140, 10150, 10160, 10170, 10190, 10211, 10212, 10220, 
                   10250, 10270, 10280)
arrowtooth_flounder <- c(10110)
P_Halibut <- c(10120)
FH_sole <- c(10130)
Dover_sole <- c(10180)
Rex_sole <- c(10200)
YF_Sole
AK_Plaice
S_rock_sole
N_rock_sole
sandlance                
sablefish
Bathylagidae
Myctophidae              
herring
other_managed_forage_fish
grenadiers
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
coho salmon
pink salmon
chum salmon              
sockeye salmon
eelpouts
other Sebastes
shortspine thornyheads
rougheye rockfish
POP          
dusky rockfish
northern rockfish
sharpchin rockfish
shortraker rockfish
hydroids        
jellyfish unidentified   
sea nettles         
egg yolk jelly     
Aequoria         
Aurelia      
lions mane jellyfish  
corals           
sea pens      
anemones        
gelatinous filter feeders
polychaetes           
benthic amphipods    
misc crustaceans         
euphausiids   
mysids   
NP shrimp  
Pandalidae        
misc crabs        
Tanner crab              
hermit crabs     
King crab           
snails                   
bivalves     
squids        
octopi                   
sea stars   
urchins-dollars-cucumbers 
brittle stars            
sponges  
misc worms    
Urochordata   

# Add column with the functional grouping to the dataframe


# Aggregate by functional groupings
Gfsh <- Gfsh %>%
            


# What Jim R. did: 
# 1 aggregated functional groups, 
# 2 assign 0 values to sampled hauls with no catch, and 
# 3 some preliminary filtering of data to the selected spatial domain.




