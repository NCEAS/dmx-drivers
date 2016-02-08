

# load necessary packages
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)


source("Benthic_Nearshore_Drivers/Benthic_Nearshore_Data_.R")
head(BenNear) 

# summarize to Site level
BenNear2 <- BenNear %>%
            select(-Quadrat, -Lat,-Long) %>%
            mutate_each(funs(as.numeric), SeaOtt_CarcToothAge) %>%
            group_by(Year, Region, Site_Name) %>%
            summarize_each(funs(sum)) %>%
            ungroup() 

# load more packages that created conflicts when loaded before sourcing data
library(reshape2)
library(ggplot2)
library(scales)
library(psych)
library(rworldmap)
library(rworldxtra)
library(rgdal)


# Make Site Map
BenNear1 <- BenNear %>%
            mutate(Region_Site = paste(Region, Site_Name, sep=":"))
# state:
state <- readOGR(dsn="Benthic_Nearshore_Drivers",layer="statep010")
stateDf <- fortify(state)
  
# palette:
colMap <- c("dimgrey","black")

colors <- c("blue3","turquoise2","deepskyblue","royalblue1","violet","thistle1",
            "darkseagreen","greenyellow","olivedrab3",
            "coral","tomato3","orangered4","rosybrown1","hotpink1",
            "yellow","goldenrod1","tan2")

# plot:
 ggplot(data=stateDf, aes(y=lat, x=long)) +
   geom_map(map=stateDf,aes(x=long,y=lat,map_id=id)) +
   coord_map(xlim = c(-155.5, -144),ylim = c(57.5, 62)) + 
   scale_fill_manual(values=colMap) +
   geom_point(data=BenNear1, aes(x=as.numeric(Long), y=as.numeric(Lat),
                                 colour=Region_Site), size=5, shape=18) + 
   facet_wrap(~Region) +
   theme(axis.line=element_line('black'),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.border=element_blank(),
         panel.background=element_blank(),
         legend.key = element_blank(),
         axis.text=element_text(size=14),
         title=element_text(size=16,face="bold")) 
  


# make Yes/No for sample presence
melt_BenNear2 <- melt(as.data.frame(BenNear2), id.vars=c("Year","Region","Site_Name"), variable_name="Data_Set")

BenNear3 <- melt_BenNear2 %>%
            mutate(Bin_Value = ifelse((is.na(value)),'0',
                               ifelse((!is.na(value)),'1',""))) %>%
            rename(DataSet=variable) %>%
            mutate_each(funs(as.character), Site_Name) %>%
            mutate_each(funs(as.character), DataSet)

BenNear4 <- BenNear3 %>%
            filter(DataSet %in% c("Leukoma_MnBmss_gWW","Macoma_MnBmss_gWW","Saxidomus_MnBmss_gWW")) %>%
            mutate_each(funs(as.numeric), Bin_Value) %>%
            group_by(Year, Region, Site_Name) %>%
            summarise(BinValue2 = sum(Bin_Value)) %>%
            ungroup() %>%
            mutate(DataSet = "Clam_Bmss_gWW")

BenNear5 <- BenNear3 %>%
            filter(DataSet %in% c("SOf_CrabSumBmss_gWW","SOf_ClamSumBmss_gWW","SOf_UrchinSumBmss_gWW",
                                  "SOf_MusselSumBmss_gWW","SOf_StarSumBmss_gWW","SOf_SnailSumBmss_gWW",
                                  "SOf_ChitonSumBmss_gWW","SOf_OctopusSumBmss_gWW","SOf_WormSumBmss_gWW",
                                  "SOf_OtherSumBmss_gWW")) %>%
            mutate_each(funs(as.numeric), Bin_Value) %>%
            group_by(Year, Region, Site_Name) %>%
            summarise(BinValue2 = sum(Bin_Value)) %>%
            ungroup() %>%
            mutate(DataSet = "SeaOttForage_Bmss_gWW")

BenNear6 <- BenNear3 %>%
            filter(DataSet %in% c("Bare_Sub_Per_Cov","barnacle_Per_Cov","mussel_Per_Cov",
                                  "Fuc_dist_Per_Cov","Ala_marg_Per_Cov","Neo_Odon_sp_Per_Cov",
                                  "Brwn_alg_ann_Per_Cov","Green_alg_ann_Per_Cov","Red_alg_ann_Per_Cov",
                                  "Red_alg_per_Per_Cov")) %>%
            mutate_each(funs(as.numeric), Bin_Value) %>%
            group_by(Year, Region, Site_Name) %>%
            summarise(BinValue2 = sum(Bin_Value)) %>%
            ungroup() %>%
            mutate(DataSet = "Intertidal_Per_Cov")
  
BenNear7 <- BenNear3 %>%
            filter(!(DataSet %in% c("Leukoma_MnBmss_gWW","Macoma_MnBmss_gWW","Saxidomus_MnBmss_gWW",
                                    "SOf_CrabSumBmss_gWW","SOf_ClamSumBmss_gWW","SOf_UrchinSumBmss_gWW",
                                    "SOf_MusselSumBmss_gWW","SOf_StarSumBmss_gWW","SOf_SnailSumBmss_gWW",
                                    "SOf_ChitonSumBmss_gWW","SOf_OctopusSumBmss_gWW","SOf_WormSumBmss_gWW",
                                    "SOf_OtherSumBmss_gWW","Bare_Sub_Per_Cov","barnacle_Per_Cov",
                                    "mussel_Per_Cov","Fuc_dist_Per_Cov","Ala_marg_Per_Cov",
                                    "Neo_Odon_sp_Per_Cov","Brwn_alg_ann_Per_Cov","Green_alg_ann_Per_Cov",
                                    "Red_alg_ann_Per_Cov","Red_alg_per_Per_Cov","SOf_CrabPropBmss",
                                    "SOf_ClamPropBmss","SOf_UrchinPropBmss","SOf_MusselPropBmss",
                                    "SOf_StarPropBmss","SOf_SnailPropBmss","SOf_ChitonPropBmss",
                                    "SOf_OctopusPropBmss","SOf_WormPropBmss","SOf_OtherPropBmss",
                                    "Leukoma_Abun_m2","Leukoma_MnSize_mm","Macoma_Abun_m2","Macoma_MnSize_mm",
                                    "Saxidomus_Abun_m2","Saxidomus_MnSize_mm"))) 
           
        
BenNear8 <- bind_rows(BenNear7,BenNear4) 
BenNear9 <- bind_rows(BenNear8,BenNear5)
BenNear10 <- bind_rows(BenNear9,BenNear6)

BenNear11 <- BenNear10 %>%
             mutate(Bin_Value = as.numeric(Bin_Value),
                    BinValue2 = as.numeric(BinValue2),
                    BinValue3 = ifelse(is.na(Bin_Value), BinValue2, Bin_Value),
                    BinValue3 = ifelse(BinValue3 > 0, 1, BinValue3),
                    Region_Site = paste(Region, Site_Name, sep=":")) %>%
             arrange(Region_Site, Year)


sample_n(BenNear11, 20)                            

  
##################
# Full plot
p <- ggplot(data=BenNear11, aes(x=Region_Site, y=Year)) + 
            geom_tile(aes(fill = as.factor(BinValue3)), colour = "white") +
            scale_fill_manual(breaks=c("0", "1"), values=c("gray", "green"), guide=FALSE) +
            scale_y_reverse() + facet_wrap(~DataSet) +
            theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1, color="black", size=10),
                  axis.text.y = element_text(color="black", size=15),
                  axis.title  = element_text(face="bold", size=20))
p

###################
# Only include Regions WPWS, KEFJ, KATM
BenNear12 <- BenNear11 %>%
             filter(Region %in% c("WPWS","KEFJ","KATM")) %>%
             arrange(Region_Site, Year)


p15 <- ggplot(data=BenNear12, aes(x=Region_Site, y=Year)) + 
              geom_tile(aes(fill = as.factor(BinValue3)), colour = "white") +
              scale_fill_manual(breaks=c("0", "1"), values=c("gray", "green"), guide=FALSE) +
              scale_y_reverse() + facet_wrap(~DataSet) +
              theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1, color="black", size=12),
                    axis.text.y = element_text(color="black", size=15),
                    axis.title  = element_text(face="bold", size=20))
p15

###################

p16 <- ggplot(data=BenNear12, aes(x=DataSet, y=Year)) + 
              geom_tile(aes(fill = as.factor(BinValue3)), colour = "white") +
              scale_fill_manual(breaks=c("0", "1"), values=c("gray", "green"), guide=FALSE) +
              scale_y_reverse() + facet_wrap(~Region_Site) +
              theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1, color="black", size=12),
                    axis.text.y = element_text(color="black", size=15),
                    axis.title  = element_text(face="bold", size=20))
p16

###################
BenNear13 <- BenNear12 %>%
             select(-Bin_Value, -BinValue2, -BinValue3) %>%
             spread(DataSet, value) # re-cast data



###################
# pairs plots

BenNear2 <- as.data.frame(BenNear2)

pairs.panels(BenNear2[,c(4:10)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T) #
pairs.panels(BenNear2[,c(37,39,41,43,45,47,49,51,53,55)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(BenNear2[,c(8,57)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T) 
pairs.panels(BenNear2[,c(8:10,15)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T) 


