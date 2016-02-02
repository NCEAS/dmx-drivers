

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


library(reshape2)
library(ggplot2)
library(scales)


# make Yes/No for sample presence
melt_BenNear2 <- melt(as.data.frame(BenNear2), id.vars=c("Year","Region","Site_Name"), variable_name="Data_Set")

BenNear3 <- melt_BenNear2 %>%
            mutate(Bin_Value = ifelse((is.na(value)),'0',
                               ifelse((!is.na(value)),'1',""))) %>%
            rename(DataSet=variable)




p <- ggplot(data=BenNear3, aes(x=Site_Name, y=Year)) + 
            geom_tile(aes(fill = Bin_Value), colour = "white") +
            scale_fill_manual(values=c("0"="white", "1"="red"), guide=FALSE) +
            scale_y_reverse() + facet_wrap(~DataSet) +
            theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1, color="black", size=15),
                  axis.text.y = element_text(color="black", size=15),
                  axis.title  = element_text(face="bold", size=20))
p



# Exploring data
library(psych)
pairs.panels(B3[,c(4:10)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(10:16)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(17:23)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(24:30)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(31:37)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(38:45)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(46:52)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
pairs.panels(B3[,c(53:56)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)



