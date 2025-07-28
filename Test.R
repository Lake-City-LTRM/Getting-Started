


# note to nicole from nicole: scroll down to where it says "START HERE"


# see R tidy text book: https://r4ds.hadley.nz/whole-game

#### install packages ####
# install.packages("tidyverse") # run this line if you need to install tidyverse
# install.packages("styler") # run this line if you need to install styler
install.packages("splitstackshape")
#### Load Packages ####
library(tidyverse) # load tidyverse package
#library(ggthemes) # contains colorblind friendly color palettes
library(splitstackshape)
library(plyr)
library(dplyr)

#### Load Data ####
fishdata <- read_csv("ltrm_fish_data.csv") # Read In Fish Data
samplepop<-read_csv("FishDesignBasedEst.csv") # Read In Sample Pop Data 


#### Organize Data ####
# clean data a bit
fishdata$sdate<-mdy(fishdata$sdate) #convert to date column type
fishdata1 <- as.data.frame(fishdata)
fishdata2 <- fishdata1[!(fishdata1$fishcode %in% c(NA)),]
fishdata3 <- fishdata2[!(fishdata2$catch %in% c(NA)),]



# create table of annual Mass per unit effort efishing lentic fish catch by strata
# create data table of just lentic fish, as listed in Status and Trends 2022
lentic <- filter(fishdata3,fishcode=="LMBS"|fishcode == "BLGL"|fishcode=="BWFN"|
                   fishcode=="WDSN"|fishcode=="NTPK"|fishcode=="BKCP"|
                   fishcode=="LNGR"|fishcode=="WTCP"|fishcode=="YWPH"|
                   fishcode=="GDSN"|fishcode=="STGR"|fishcode=="GNSF"|
                   fishcode=="PNSD"|fishcode=="OSSF") 
## first try w/o removing periods...
#lentic2<-lentic[!(lentic$period==1),] ######################### why did I remove period 1??
#lentic3<-lentic2[!(lentic2$period==4),] ######################## what is period 4? seems good to remove

#remove fish with weight <=0 (1/5 of data)
LTRM_bm<-lentic [which(weight>0), ]
#subset master databse variables
LTRM_bm2 <- LTRM_bm[,c(3, 6, 73, 74, 78)]

#add log length
LTRM_bm2$loglength <- log10( LTRM_bm2$length)
#add log weight
LTRM_bm2$logweight <- log10(LTRM_bm2$weight)

#summarize
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
LTRM_n_spp<-summarySE(LTRM_bm2, measurevar="loglength", groupvars=("fishcode"))
#based on weight data available, no additional species can be updated within the life history database

#estimate ordinary least square regression coefficients for LMBS (CHANGED FROM BOUSKA CNCF) 
# estimates - is it worth updating the life history database?
LTRM_LMBS<-LTRM_bm2 [which(LTRM_bm2$fishcode=="LMBS"), ]
lmodel <- lm(logweight ~ loglength, data = LTRM_LMBS)
lmodel$coefficients

plot(LTRM_LMBS$logweight~LTRM_LMBS$loglength)
plot(LTRM_LMBS$weight~LTRM_LMBS$length)


lentic4<-lentic %>% 
  filter(stratum=="MCB-U"|stratum=="SCB"|stratum=="BWC-S") %>% 
  mutate(stratum = if_else(stratum=="SCB","SC",stratum)) %>% 
  filter(gear=="D") %>%  ## only day electrofishing
  mutate(stratum = if_else(stratum =="MCB-U"& gear=="D","MCB-S",stratum))   # from https://www.umesc.usgs.gov/ltrmp/stats/code/sampling_weights_BLGL_day.sas

#lentic5<-lentic4 %>%  
# select(sdate,barcode,stratum,effmin,fishcode,catch, weight)%>% 
#  filter(catch>0) ## maybe don't want to filter to catch > 0 because zeros matter to summary metrics?
#lentic6<-expandRows(lentic4,"catch")

#expand rows
LTRM2<-filter(lentic4,catch>0) ## from Bouska code -- ask her about removing these zeros (sites not sampled and then alt chosen?)
LTRM2<-expandRows (LTRM2, "catch")
## from bouska code "length_weight approximations.R"
LTRM2$sdate <-as.Date(LTRM2$sdate, '%m/%d/%Y')
LTRM2$year<-format(LTRM2$sdate,"%Y")
LTRM2<-LTRM2 [which(LTRM2$year>1992), ]

#add log length
LTRM2$loglength <- log10( LTRM2$length)
#subset master database variables
LTRM3 <- LTRM2[,c(2,3,5, 6,19, 73, 74, 76, 77,86)]

#query electrofishing data
LTRM1<-LTRM [which(gear=="D"), ]
#query for SRS data
LTRM1$sdate <-as.Date(LTRM1$sdate, '%m/%d/%Y')
LTRM1$year<-format(LTRM1$sdate,"%Y")
LTRM2<-LTRM1 [which(LTRM1$year>1992), ]
#expand rows
LTRM2<-filter(LTRM2,catch>0)
LTRM2<-expandRows (LTRM2, "catch")
#add log length
LTRM2$loglength <- log10( LTRM2$length)
#subset master database variables
LTRM3 <- LTRM2[,c(2,3,5, 6,19, 73, 74, 77, 78,87)]

#next steps
#load lw equation data
LW =LTRM_n_spp
#load lw equation data

## START HERE NICOLE from 28 July 2025
LW =read.delim2(file="lifehistory_growth.txt", header=TRUE, sep=",")### NEED TO GET THIS!!!! from lmodel above, for each species!!!!
#rename Fishcode to fishcode in LW
colnames(LW)[colnames(LW)=="Fishcode"] <- "fishcode"
#join by fishcode
library(dplyr)
LTRM4<-inner_join(LTRM3, LW, by = c("fishcode" = "fishcode"))
#subset database variables
LTRM4 <- LTRM4[,c(1,2,3,4,5, 6, 7,8, 9,10,11, 12)]
#change factors to numeric
str(LTRM4)
LTRM4$LW.Intercept<-as.numeric(as.character(LTRM4$LW.Intercept))
LTRM4$LW.Slope<-as.numeric(as.character(LTRM4$LW.Slope))
#calculate biomass per record
LTRM4$est_logwt <- LTRM4$LW.Intercept + (LTRM4$LW.Slope*LTRM4$loglength)
LTRM4$est_wt<-(10^LTRM4$est_logwt)


#compare weights vs. est.wts (tend to overestimate little fish and underestimate big fish)
LTRM5 <-LTRM4 [which(LTRM4$weight>0), ]
LTRM5$wt_diff<-LTRM5$weight - LTRM5$est_wt
plot(LTRM5$wt_diff~LTRM5$weight)
hist(LTRM5$weight)

#estimate biomass by multiplying catch*est_wt
LTRM4$bm<-LTRM4$est_wt*LTRM4$catch
#correct for effort
LTRM4 <- within (LTRM4, effmin[effmin==0] <- 15)
LTRM4$mpue<-LTRM4$bm*(15/LTRM4$effmin)
#summarize by barcode - total biomass by species (mpue)
LTRM_bm_sum<- LTRM2 %>%
  group_by(barcode, fishcode) %>%
  summarise (bm_sum = sum(mpue, na.rm = TRUE))
#sum mpue by stata, fstation, year (to calculate mean mpue per strata) FROM BOUSKA
LTRM_bm_srs_strata<-LTRM2 %>%
  group_by(stratum, year, fstation, fishcode) %>%
  summarize(sum.mpue = sum(bm_sum, na.rm = TRUE))
 
  mutate(mass = as.numeric(weight)) %>% 
  filter(!is.na(mass)) %>% 
  mutate(mpue = mass/effmin*15)

# start BOUSKA

#summarize by barcode - total biomass by species (mpue)
LTRM_bm_sum<- lentic5 %>%
  group_by(barcode, fishcode) %>%
  summarise (bm_sum = sum(mpue, na.rm = TRUE))

#add sample information back in
LTRM_sampleinfo <- lentic4[,c(1,2,3,4,5,6)]
LTRM_sampleinfo2<-unique(LTRM_sampleinfo)
LTRM_bm_sum2<-inner_join(LTRM_bm_sum, LTRM_sampleinfo2, by = c("barcode" = "barcode"))
LTRM_bm_sum2$sdate <-as.Date(LTRM_bm_sum2$sdate, '%m/%d/%Y')
LTRM_bm_sum2$year<-format(LTRM_bm_sum2$sdate,"%Y")

#sum mpue by stata, fstation, year (to calculate mean mpue per strata)
LTRM_bm_srs_strata<-LTRM_bm_sum2 %>%
  group_by(stratum, year, fstation, fishcode) %>%
  summarize(sum.mpue = sum(bm_sum, na.rm = TRUE))

#summarize # of barcodes per reach, year, stratum combination (to calculate mean mpue per strata)
barcodes<- LTRM_bm_sum2 %>%                    # take the data.frame "data"
  group_by(stratum, year, fstation) %>%          # Then, with the filtered data, group it by "bb"
  summarise(Unique_barcode = n_distinct(barcode))   # Now summarise with unique elements per group

#join with srs_strata table
LTRM_bm_srs_strata_mean<-inner_join(LTRM_bm_srs_strata, barcodes, by = c("stratum" = "stratum", "fstation"="fstation", "year"="year" ))
#calculate mean mpue per strata (yh) 
LTRM_bm_srs_strata_mean$mpue<-(LTRM_bm_srs_strata_mean$sum.mpue/LTRM_bm_srs_strata_mean$Unique_barcode)

#load population data
pop =read.csv(file="FishDesignBasedEst.csv", header=TRUE, sep=",")
pop$stratum<-pop$ï..Stratum
pop <- within (pop, stratum[stratum=="SC-S"] <- "SC")

LTRM_bm_srs_strata_pop<-inner_join(LTRM_bm_srs_strata_mean, pop, by = c("stratum" = "stratum" ))
#multiply mpue *Nh
LTRM_bm_srs_strata_pop$mpueXNh <- LTRM_bm_srs_strata_pop$mpue * LTRM_bm_srs_strata_pop$PopSize

#sum mpueXNh across stratum per fishcode, fstation, year
LTRM_bm_srs_poolwide<-LTRM_bm_srs_strata_pop %>%
  group_by( year,fstation) %>%
  summarize(sum.mpueXNh = sum(mpueXNh, na.rm = TRUE),
            sd.mpuexNh = sd(mpueXNh,na.rm = TRUE)) %>% 
  filter(year>1992&year<2020)


#add column with N
LTRM_bm_srs_poolwide$N <- 0
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='1'] <- 6343)
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='2'] <- 7710)
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='3'] <- 6984)
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='4'] <- 6638)
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='5'] <- 3669)
LTRM_bm_srs_poolwide <- within (LTRM_bm_srs_poolwide, N[fstation=='6'] <- 9116)
# calculate poolwide mpue
LTRM_bm_srs_poolwide$pw.mpue<-(LTRM_bm_srs_poolwide$sum.mpueXNh/LTRM_bm_srs_poolwide$N)

# end BOUSKA
## use the CATCH column for redundant rows for when they don't weigh every fish
## #PLOT it NICOLE!

lentic_annualsum <- lentic5 %>% 
  mutate(year = year(sdate)) %>%  #create year column
  group_by(year,stratum) %>%      # group by year and strata
  summarise(AnnMeanMPUE = sum(MPUE),Asd = sd(MPUE))# calculate total catch per strata each year
  
### LENTIC FISH PLOT RECREATION FIGURE G3 from S&T
ggplot(data = LTRM_bm_srs_poolwide,
       mapping = aes(x=year,y=pw.mpue))+
  geom_point()+
 # geom_errorbar(aes(ymin=(AnnMeanMPUE-Asd),ymax=(AnnMeanMPUE+Asd)))+
  geom_smooth(method = "lm")+
  ggtitle("Annual Average MPUE (15 min e)")+
  theme_bw()
# create table of annual total  fish catch by strata

fishdata4<-fishdata3 %>% 
  select(sdate,stratum,catch,fishcode) %>% 
  filter(stratum=="MCB-U"|stratum=="SCB"|stratum=="BWC-S"|stratum=="BWC-O")

tot_annualsum <- fishdata4 %>% 
  mutate(year = year(sdate)) %>%  #create year column
  group_by(year,stratum) %>%      # group by year and strata
  summarise(TotCatch = sum(catch))# calculate total catch per strata each year

tot_annualsum2 <- fishdata4 %>% 
  mutate(year = year(sdate)) %>%  #create year column
  group_by(year) %>%      # group by year and strata
  summarise(TotCatch = sum(catch))# calculate total catch each year

#### Visualize Data ####
ggplot(data = tot_annualsum,
       mapping = aes(x=year,y=TotCatch,color = stratum))+
  geom_point()+
  geom_smooth()+
  ggtitle("Total Fish Catch by Year and Stratum")+
  theme_bw()
ggplot(data = tot_annualsum2,
       mapping = aes(x=year,y=TotCatch))+
  geom_point()+
  geom_smooth(method='lm')+
  ggtitle("Total Fish Catch by Year")+
  theme_bw()



