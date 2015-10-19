###########################################################
##### Data Cleaning Script - DMX Benthic Nearshore
##### Invertebrates and Algae (intertidal point counts): 
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
# Inverts & Algae Percent Cover
URL_IA <- "https://workspace.aoos.org/published/file/744b4b95-e596-41cc-9419-881165da2864/BenthicNearshoreSystemsInGOA_SOP04_RockyCover_2006to2014_Data_20141015.csv"
IAGet <- GET(URL_IA)
IA1 <- content(IAGet, as='text')
IA <- read.csv(file=textConnection(IA1))
head(IA)

# Cleaning
# define common categories of species for larger aggregation of data
anemone <-  c("Anthopleura elegantissima","Anthopleura xanthogrammica","Epiactis sp.",
              "Metridium senile","unidentified anemone","Urticina crassicornis")
barnacle <- c("Balanus / Semibalanus sp.","Balanus glandula","barnacle","barnacle spat",
              "Chthamalus dalli","Semibalanus balanoides","Semibalanus cariosus")
brown_alga <- c("Alaria marginata","Analipus japonicus","Chordaria flagelliformis",
                "Coilodesme bulligera","Desmarestia aculeata","Dictyosiphon foeniculaceus",
                "Ectocarpus sp.","Elachista fucicola","Elachista sp.","Eudesme virescens",
                "Fucus distichus","Leathesia marina","Melanosiphon / Scytosiphon sp.",
                "Melanosiphon intestinalis","Petalonia fascia","Ralfsia fungiformis",
                "Ralfsia sp.","Saccharina latissima","Saccharina sessilis",
                "Scytosiphon lomentaria","Soranthera ulvoidea","unidentified brown algae")
bryazoan <- c("encrusting bryozoan","Eurystomella bilabiata","foliose bryozoan",
              "Stomachetosella cruenta")
chiton <- c("Cryptochiton stelleri")
clam <- c("Hiatella arctica","Mya truncata")
coralline_alga <- c("Corallina sp.","encrusting coralline algae","foliose coralline algae")
encrusting_red_alga <- c("Hildenbrandia sp.","non-coralline algal crust")
#ephemeral_aglae <- (???????????????????????????????????????????)
filamentous_brown <- c("Pylaiella littoralis")
filamentous_green <- c("Chaetomorpha melagonium","Chaetomorpha sp.","Cladophora / Chaetomorpha sp.",
                       "Ulothrix flacca")
green_alga <- c("Acrosiphonia sp.","Blidingia minima var. minima","Ulva / Monostroma sp.",
                "Ulva sp.","unidentified green algae")
hydroid <- c("unidentified hydroid")
jingle_shell <- c("Pododesmus macroschisma")
mussel <- c("Modiolus modiolus","Musculus sp","Mytilus trossulus")
red_alga <- c("Ahnfeltia fastigiata","Antithamnionella pacifica",
              "Boreophyllum / Pyropia / Wildemania sp.","Callithamnion pikeanum",
              "Ceramium pacificum","Constantinea subulifera","Cryptopleura ruprechtiana",
              "Cryptosiphonia woodii","Dumontia alaskana","Endocladia muricata",
              "Gloiopeltis furcata","Gracilaria pacifica","Halosaccion glandiforme",
              "Mastocarpus sp.","Mazzaella parksii","Mazzaella phyllocarpa","Mazzaella sp.",
              "Microcladia borealis","Nemalion elminthoides","Neoptilota / Ptilota sp.",
              "Neorhodomela larix","Neorhodomela oregona","Odonthalia / Neorhodomela sp.",
              "Odonthalia floccosa","Palmaria callophylloides","Palmaria hecatensis",
              "Palmaria hecatensis/mollis","Palmaria mollis","Palmaria sp.",
              "Phycodrys / Tokidadendron sp.","Phycodrys fimbriata","Pleonosporium vancouverianum",
              "Plocamium pacificum","Polysiphonia sp.","Pterosiphonia / Polysiphonia sp.",
              "Pterosiphonia bipinnata","Ptilota sp.","Rhodochorton purpureum",
              "Tokidadendron bullatum","unidentified filamentous red algae")
sponge <- c("unidentified sponge")
tunicate <- c("unidentified tunicate")
worm <- c("spirorbidae","unidentified worm")


IA_GOA <- IA %>% 
          filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014, 2015)) %>% # taking out years before 2010
          rename(Year=Sample_Year, Quadrat=Quadrat_Num) %>%
          mutate(Common_Cat = ifelse((Species_Name %in% anemone),'anemone',
                              ifelse((Species_Name %in% barnacle),'barnacle',
                              ifelse((Species_Name %in% brown_alga),'brown_alga',
                              ifelse((Species_Name %in% bryazoan),'bryazoan',
                              ifelse((Species_Name %in% chiton),'chiton',
                              ifelse((Species_Name %in% clam),'clam',
                              ifelse((Species_Name %in% coralline_alga),'coralline_alga',
                              ifelse((Species_Name %in% encrusting_red_alga),'encrusting_red_alga',
                              #ifelse((Species_Name %in% ephemeral_aglae),'ephemeral_aglae',
                              ifelse((Species_Name %in% filamentous_brown),'filamentous_brown',
                              ifelse((Species_Name %in% filamentous_green),'filamentous_green',
                              ifelse((Species_Name %in% green_alga),'green_alga',
                              ifelse((Species_Name %in% hydroid),'hydroid',
                              ifelse((Species_Name %in% jingle_shell),'jingle_shell',
                              ifelse((Species_Name %in% mussel),'mussel',
                              ifelse((Species_Name %in% red_alga),'red_alga',
                              ifelse((Species_Name %in% sponge),'sponge',
                              ifelse((Species_Name %in% tunicate),'tunicate',
                              ifelse((Species_Name %in% worm),'worm',"")))))))))))))))))))
                 )     # add new column with common category
head(IA_GOA) ; IA_GOA[45:90,]

##### 
##### 
# FUNCTION for getting Percent Cover for all intertidal inverts and algae
PerCovCalc <- function(df, new_column_name) { 
              # calculate the total number of points used in each quadrat
              x <- IA_GOA %>% 
                   filter(Species_Name %in% c("Bare Substrate","bare substrate")) %>% 
                   count(Site_Code, Site_Name, Year, Elevation_Position, Quadrat) %>%
                   rename(Point_Count=n)
              # join calculated points df to referenced df
              df1 <- left_join(df, x, by=c("Site_Code", "Site_Name", "Year", 
                                           "Elevation_Position", "Quadrat"))
              # calculate percent cover per quadrat
              df1 <- df1 %>%
                     count(Site_Code, Site_Name, Year, Elevation_Position, Quadrat, Point_Count) %>%
                     mutate(Per_Cov = (n/Point_Count)*100) %>% 
                     group_by(Site_Name, Year, Quadrat) %>%
                     summarise_(.dots = setNames(list(~mean(Per_Cov)), new_column_name)) %>%  # mean of elevations together
                     ungroup() %>% 
                     select_("Site_Name", "Year", "Quadrat", new_column_name) %>%
                     arrange(Site_Name, Year, Quadrat)
              return(df1)
              }
#####
#####

# Bare Substrate 
BS_IA_1 <- filter(IA_GOA, Species_Name=="Bare Substrate", Layer_Num=="1") # only in layer 1 according to Tom Dean
BS_IA <- PerCovCalc(BS_IA_1, "Bare_Sub_Per_Cov")    

# Barnacles
b_IA_1 <- filter(IA_GOA, Common_Cat=="barnacle")   # Filter out the species/entry of interest
b_IA <- PerCovCalc(b_IA_1, "barnacle_Per_Cov") # call the function

# Mussels
ms_IA_1 <- filter(IA_GOA, Common_Cat=="mussel")   # Filter out the species/entry of interest
ms_IA <- PerCovCalc(ms_IA_1, "mussel_Per_Cov")  # call the function

# Fucus distichus
Fd_IA_1 <- filter(IA_GOA, Species_Name=="Fucus distichus")   # Filter out the species/entry of interest
Fd_IA <- PerCovCalc(Fd_IA_1, "Fuc_dist_Per_Cov")  # call the function

# Alaria marginata
Am_IA_1 <- filter(IA_GOA, Species_Name=="Alaria marginata")   # Filter out the species/entry of interest
Am_IA <- PerCovCalc(Am_IA_1, "Ala_marg_Per_Cov")  # call the function

# Neorhodomela sp
Neo_sp <- c("Neorhodomela oregona","Neorhodomela larix","Odonthalia / Neorhodomela sp.","Odonthalia floccosa")
No_IA_1 <- filter(IA_GOA, Species_Name %in% Neo_sp)   # Filter out the species/entry of interest
No_IA <- PerCovCalc(No_IA_1, "Neo_sp_Per_Cov") # call the function



## IS THIS WHAT IS MEANT BY EPHEMERAL ALGAE??
# Ephemeral Algae
ephem_alg <- c("filamentous_brown", "filamentous_green", "green_alga")
ephm_IA_1 <- filter(IA_GOA, Common_Cat %in% ephem_alg)   # Filter out the species/entry of interest
ephm_IA <- PerCovCalc(ephm_IA_1, "ephemeral_algae_Per_Cov")  # call the function







