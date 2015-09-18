## Ecopath model input spreadsheet data formatting
## Jessica Couture
## started 9 Jul 2015

## EwE table format:
## Name | sp1measure | sp2measure
## pool code
## type
## yr1 | yrAbund | 
## yr2 | yrAbubd | 
## yr3...| yrAbund | 

library(dataone)
library(dplyr)
library(gdata)
library(reshape2)

cm <- CertificateManager()
user <- showClientSubject(cm)

 
pwsEwe=data.frame(
  'Name'=factor(c('poolCode','type',1955:2015,'units'),levels=c('poolCode','type',1955:2015,'units'),ordered=T))


mn_uri<-"https://goa.nceas.ucsb.edu/goa/d1/mn/v1"  ## define goa portal as DataONE member node
mn <- MNode(mn_uri)

######### Humpback whale abundances - Teerlink
## Type = 1 (absolute abundances) --> What do I do about them being estimates, uncertainty?

hbwId <- "df35d.123.1"   # unique identifier for this data file
hbwObj <- get(mn,hbwId)
hbw <- read.csv(text=rawToChar(hbwObj))
hbw2=hbw %>%
  mutate(Name=as.factor(Year)) %>%
  mutate(humpbackWhaleBM=hbw[,3]*29000*0.001) %>%
  select(Name,humpbackWhaleBM)

pwsEwe=merge(pwsEwe,hbw2,all.x=T) ## x29,000 kg to get BM estimate. NOAA states hbw are 22,000-36,000kg each: http://www.nmfs.noaa.gov/pr/species/mammals/whales/humpback-whale.html
pwsEwe[2,2]=0
pwsEwe[64,2]='tonnes'

######### Sea otter abundance estimates
## Type = 1 (absolute abundances) --> What do I do about them being estimates, uncertainty?

soTab=read.csv('eweData/sea_otter_abundance_estimates_PWS_1994-2014_for_NCEAS_28May2015.csv',stringsAsFactors=F)
soDtSpl=strsplit(soTab[,1],split=' ')
soEst=data.frame(Name=sapply(soDtSpl,function(x) x[1]),seaOtterBM=as.numeric(soTab[,2])*30*0.001) ## http://www.marinemammalcenter.org/education/marine-mammal-information/sea-otter.html; 23kg (Okey & Pauly 1999)

new=merge(pwsEwe,soEst,all.x=T)
new[2,3]=0
pwsEwe[64,3]='tonnes'

######### Steller sea lions

sslId <- 'df35b.270.1'   # unique identifier for non-pup SSL data
sslObj <- get(mn, sslId)
ssl <- read.csv(text=rawToChar(sslObj))
# pws bounds: 59.922623, -147.947102, 61.159456, -146.336512
sslPws=ssl %>%
  filter(Lat>59.922623) %>%
  filter(Lat<61.159456) %>%
  filter(Long> -147.947102) %>%
  filter(Long< -146.336512) %>%
  group_by(year) %>%
  summarise(aveSSL=mean(adult.juvenileCount)) %>%
  mutate(sslBM=3.43*735*aveSSL*0.001) ## population estimate equation from Trites & Larkin 1996, ave adult weight = 735 from The Marine Mamma Center: http://www.marinemammalcenter.org/education/marine-mammal-information/pinnipeds/steller-sea-lion/ Same site used for pup weights.

pupId <- 'df35b.271.1'   # unique identifier for pup SSL data
pupObj <- get(mn, pupId)
pups <- read.csv(text=rawToChar(pupObj))
# pws bounds: 59.922623, -147.947102, 61.159456, -146.336512
pupsPws=pups %>%
  filter(Lat>59.922623) %>%
  filter(Lat<61.159456) %>%
  filter(Long> -147.947102) %>%
  filter(Long< -146.336512) %>%
  group_by(year) %>%
  summarise(avePup=mean(pupcount)) %>%
  mutate(pupBM=4.64*20*avePup*0.001) # conversion (4.64) from Trites & Larkin 1996; ave weight ~ 20kg

pwsSslPop=merge(sslPws,pupsPws,all.x=T)
pwsSslPop2=pwsSslPop%>%
  mutate(stellerSlBM=ifelse(is.na(pupBM)==T,sslBM,sslBM+pupBM)) %>%
  mutate(Name=as.factor(year)) %>%
  select(Name,stellerSlBM)

pwsEwe=merge(new,pwsSslPop2,all=T)
pwsEwe[2,4]=0
pwsEwe[64,4]='tonnes'

######### Harbor seals

hsId <- 'df35d.96.7'   # unique identifier for harbor seal data
hsObj <- get(mn, hsId)
hs <- read.csv(text=rawToChar(hsObj)) ## assuming these are unadjusted counts, take mean for year over PWS sites and multiply by 2.20 to get population estimate (Boveng et al. 2003), ave adult weight is 82kg (adf&g website, http://www.adfg.alaska.gov/index.cfm?adfg=harborseal.main)

hsSpl=strsplit(as.character(hs$date),'/')

hs2=hs%>%
  mutate(Name=sapply(hsSpl,function(x) x[3])) %>%
  filter(use=="Y") %>%
  filter(!is.na(seals)) %>%
  filter(latitude>59.922623) %>%
  filter(latitude<61.159456) %>%
  filter(longitude> -147.947102) %>%
  filter(longitude< -146.336512) %>%
  group_by(Name) %>%
  summarise(aveHS=mean(seals)) %>%
  mutate(harborSealBM=2.2*82*aveHS*0.001) %>%
  select(Name,harborSealBM)

pwsEwe=merge(pwsEwe,hs2,all.x=T)
pwsEwe[2,5]=0
pwsEwe[64,5]='tonnes'

######### Herring data: frm adfg, pulled from pfx covar. git hub page

herr=read.xls("eweData/PWS_Biomass_Summaries_1974–2014_Updated_9-5-2014.xlsx",sheet=1,pattern='year',blank.lines.skip=T,stringsAsFactors=F)
herr2=herr %>%
  filter(!is.na(Estimate.Year)) %>%
  mutate(Name=Estimate.Year) %>%
  mutate(pacHerrBM=as.numeric(gsub(',','',X.tons..1))) %>% #tons or tonnes?
  mutate(pacHerrCatches=as.numeric(gsub(',','',X.tons.))) %>%
  select(Name,pacHerrBM,pacHerrCatches)
pwsEwe=merge(pwsEwe,herr2,all.x=T)
pwsEwe[2,5:6]=c(0,6)
pwsEwe[64,5:6]='tonnes'


########## Chum estimates and harvest
chum=read.xls("eweData/2015_PWS_Wild_Chum forecast-FINAL.xls",sheet=2,pattern='Return Year',blank.lines.skip=T,stringsAsFactors=F)

aveChumWt=0.001*(4.4+10.0)/2 ## wikipedia: adults range from 4.4-10.0 kg

chum2=chum %>%
  "["(.,1:45,) %>%
  mutate(Name=Return.Year) %>%
  mutate(chumBM=as.numeric(gsub(',','',Escapement))*2+(as.numeric(gsub(',','',Harvest)))*aveChumWt) %>%
  mutate(chumCatches=as.numeric(gsub(',','',Harvest))*aveChumWt) %>%
  select(Name,chumBM,chumCatches)

pwsEwe=merge(pwsEwe,chum2,all.x=T)
pwsEwe[2,8:9]=c(0,6)
pwsEwe[64,8:9]='tonnes'

######### Pink estimates - Rich Brenner, ADF&G
pink=read.xls("eweData/2015_PWS_Pink_Wild_forecast-FINAL.xlsm",sheet=2,pattern='Brood Line',blank.lines.skip=T,stringsAsFactors=F)

avePinkWt=((3.5+5)/2)*0.454*0.001 ## NOAA: 3.5- 5lbs, ave estimated wt in lbs, convert to kg-->tonnes

pink2=pink %>%
  "["(.,59:120,) %>% ## have data from 1896 but not sure if that's useful so limiting to 1955, ask Tom
  mutate(Name=Run.Year) %>%
  mutate(pinkBM=as.numeric(gsub(',','',Total..w..obs.eff.and.prop.unsurveyed.))*avePinkWt) %>%
  mutate(pinkCatches=as.numeric(gsub(',','',CCP))*avePinkWt) %>%
  select(Name,pinkBM,pinkCatches)

pwsEwe=merge(pwsEwe,pink2,all.x=T)
pwsEwe[2,10:11]=c(0,6)
pwsEwe[64,10:11]='tonnes'


######### Tanner Crab abundances - ADF&G
## Post online and link to this 
tcrabNHM=read.xls("eweData/PWS_Tanner_Crab_Abundance_Estimates_1991-2014-1.xlsx",sheet=1,pattern='Year',blank.lines.skip=T,stringsAsFactors=F,na.strings = c('-','',' ')) ## read first sheet from tanner crab estimates: Abundances of male carbs at Nothern and hinchbrook Districts
colnames(tcrabNHM)=c('year','dataType','pre4Males','pre3Males','pre2MalesOld','pre2MalesNew','pre1MalesOld','pre1MalesNew','recruitMaleOld','recruitMaleNew','postRecruitMalesOld','postRecruitMalesNew','legalMales','matureMales','totMales') ## Pre-4= (< 73mm), Pre-3=73-92mm, Pre-2=93-112mm,	Pre-1=113-134mm, Recruit=135-157mm, Post-recruit= (>157mm), Legal Males= (>135mm),	MatureMales= (>113)
tcrabNHM=tcrabNHM[tcrabNHM$dataType=='Abundance',1:15]
tcrabNHM2<-tcrabNHM %>%
  mutate(sex='M') %>%
  mutate(site='Northern and Hinchbrook Districts')%>%
  rename(total=totMales) %>%
  select(year,sex,site,total)

tcrabNHF=read.xls("eweData/PWS_Tanner_Crab_Abundance_Estimates_1991-2014-1.xlsx",sheet=2,pattern='Year',blank.lines.skip=T,stringsAsFactors=F,na.strings = c('-','',' ')) ## read 2nd sheet from tanner crab estimates: Abundances of FEMALE carbs at Nothern and hinchbrook Districts
colnames(tcrabNHF)=c('year','dataType','juvFem','matureFem','totFem')
tcrabNHF=tcrabNHF[tcrabNHF$dataType=='Abundance',1:5]
tcrabNHF2<-tcrabNHF %>%
  mutate(sex='F') %>%
  mutate(site='Northern and Hinchbrook Districts')%>%
  rename(total=totFem) %>%
  select(year,sex,site,total)


tcrabVM=read.xls("eweData/PWS_Tanner_Crab_Abundance_Estimates_1991-2014-1.xlsx",sheet=3,pattern='Year',blank.lines.skip=T,stringsAsFactors=F,na.strings = c('-','',' ')) ## read 3rd sheet from tanner crab estimates: Abundances of male carbs in Valdez Arm
colnames(tcrabVM)=c('year','dataType','pre4Males','pre3Males','pre2MalesOld','pre2MalesNew','pre1MalesOld','pre1MalesNew','recruitMaleOld','recruitMaleNew','postRecruitMalesOld','postRecruitMalesNew','legalMales','matureMales','totMales') ## Pre-4= (< 73mm), Pre-3=73-92mm, Pre-2=93-112mm,	Pre-1=113-134mm, Recruit=135-157mm, Post-recruit= (>157mm), Legal Males= (>135mm),	MatureMales= (>113); old/new refers to shell condition
tcrabVM=tcrabVM[,1:15]
tcrabVM2=tcrabVM %>%
  filter(dataType=='Abundance') %>%
  mutate(sex='M') %>%
  mutate(site='ValdezArm')%>%
  rename(total=totMales) %>%
  select(year,sex,site,total)

tcrabVF=read.xls("eweData/PWS_Tanner_Crab_Abundance_Estimates_1991-2014-1.xlsx",sheet=4,pattern='Year',blank.lines.skip=T,stringsAsFactors=F,na.strings = c('-','',' ')) ## read 4th sheet from tanner crab estimates: Abundances of FEMALE carbs in Valdez Arm
colnames(tcrabVF)=c('year','dataType','juvFem','matureFem','totFem')
tcrabVF=tcrabVF[,1:5]
tcrabVF2=tcrabVF %>%
  filter(dataType=='Abundance') %>%
  mutate(sex='F') %>%
  mutate(site='ValdezArm')%>%
  rename(total=totFem) %>%
  select(year,sex,site,total)

tcV=merge(tcrabVF2,tcrabVM2,by='year')
tcv2=tcV %>%
  mutate(totF=gsub(',','',total.x))%>%
  mutate(totM=gsub(',','',total.y))%>%
  mutate(tannerCrabAbundVal=as.numeric(totF)+as.numeric(totM)) %>%
  mutate(Name=as.numeric(year)) %>%
  select(Name,tannerCrabAbundVal) ## Tanner crab abundance estimate from Valdez Arm, ask Rich what area this covers?

tcNH=merge(tcrabNHM2,tcrabNHF2,by='year')
tcnh2=tcNH %>%
  mutate(totF=gsub(',','',total.x))%>%
  mutate(totM=gsub(',','',total.y))%>%
  mutate(tannerCrabAbundNoHin=as.numeric(totF)+as.numeric(totM)) %>%
  mutate(Name=as.numeric(year)) %>%
  select(Name,tannerCrabAbundNoHin) ## Tanner crab abundance estimate from Northern District and Hinchbrook District, ask Rich what areas these cover?
tanCr=merge(tcv2,tcnh2,all=T)

pwsEwe=merge(pwsEwe,tanCr,all.x=T)
pwsEwe[64,c('tannerCrabAbundVal','tannerCrabAbundNoHin')]='abundance'


######### ADF&G Herring BM estimates data from Steve Moffitt/Rich Brenner 1980-2014 (ASA estimates)
## Replaced Norcross/Brown data since that was a short timeseries

#ffId <- "df35d.94.5"   # unique identifier for this data file
#ffObj <- get(mn,ffId)
#ff <- read.csv(text=rawToChar(ffObj))

ff=read.xls('eweData/tabulaHerringBMFMR15-34.csv',header=F,stringsAsFactors = T,na.strings=c('NAk','NA','ND','NAj'),strip.white = T)
colnames(ff)=c('harvMgmtYr','totSpringUseHarvMort','aerialPeakBM','aerialMaxPossBM','aerialMiSpawn','aerialMiDaysSpawn','asaUnexplEsc','asaPreFishEst','obsAcoustPeakBMFall','obsAcoustPeakBMSpr','priorYrForecast')

ff2=ff %>%
  filter(!is.na(asaPreFishEst)) %>%
  rename(Name=harvMgmtYr) %>%
  mutate(preFish=gsub(',','',asaPreFishEst)) %>%
  mutate(herrBmTonnes=as.numeric(preFish)*0.907185) %>% ##conversion from st to metricT
  select(Name,herrBmTonnes)

pwsEwe=merge(pwsEwe,ff2,all.x=T)
pwsEwe[2,"herrBmTonnes"]=0
pwsEwe[64,"herrBmTonnes"]='relativeBiomass'


######### Zooplankton SEA proj: 94-98 --> OFF SHORE Zooplankton
## TO calculated these in first model

######### PWS Zooplankton tows - Campbell
## connect to access db: http://rprogramming.net/connect-to-ms-access-in-r/
library(Hmisc)
pwsZoo <- mdb.get("eweData/LTM_PWS_Zooplankton.accdb") 
contents(pwsZoo) # 3 tables, menu, cruise data, tow data

#pzM<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='menutblTaxa') # species list
#pzCr<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='tblCruiseData') # cruise data
pzTow<-mdb.get('eweData/LTM_PWS_Zooplankton.accdb',tables='tblTowData') # Tow data, contains species concetrations per tow and year embeded in the TowID other metadata tables not needed

## Cam says no need to aggregate into omnivore/herbivore
## Campbell confirmed that the "total"column from pzTow is nIndividuals/m3

yrSpl=strsplit(as.character(pzTow$TowID),'-')
pzTow2=mutate(pzTow,Name=sapply(yrSpl,function(x) x[1]))

pzGrps=group_by(pzTow2,Name,Species)
pzt=pzGrps %>%
  summarise(plktnConc=mean(Total))%>%
  select(Name,Species,plnktnConc)

pwsPlk=dcast(pzt,Name~Species,value.var = 'plktnConc')
  
pwsEwePl=merge(pwsEwe,pwsPlk,all.x=T)
pwsEwePl[64,15:212]='concentration (indiv/m3)'
