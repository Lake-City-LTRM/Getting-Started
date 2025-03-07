
# see R tidy text book: https://r4ds.hadley.nz/whole-game

#### install packages ####
# install.packages("tidyverse") # run this line if you need to install tidyverse
# install.packages("styler") # run this line if you need to install styler
install.packages("splitstackshape")
#### Load Packages ####
library(tidyverse) # load tidyverse package
#library(ggthemes) # contains colorblind friendly color palettes
library(splitstackshape)

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
lentic2<-lentic[!(lentic$period==1),]
lentic3<-lentic2[!(lentic2$period==4),]
#subset master databse variables
lentic3p5 <- lentic3 %>% 
  select(barcode,fstation,sitetype,stratum,sdate,pool,gear,summary,effmin,
         fishcode,length,catch,weight)

lentic4<-lentic3 %>% 
  filter(stratum=="MCB-U"|stratum=="SCB"|stratum=="BWC-S") %>% 
  mutate(stratum = if_else(stratum=="SCB","SC",stratum)) %>% 
  filter(gear=="D") %>%  ## only day electrofishing
  mutate(stratum = if_else(stratum =="MCB-U"& gear=="D","MCB-S",stratum))   # from https://www.umesc.usgs.gov/ltrmp/stats/code/sampling_weights_BLGL_day.sas

lentic5<-lentic4 %>%  
 select(sdate,barcode,stratum,effmin,fishcode,catch, weight)%>% 
  filter(catch>0)
lentic6<-expandRows(lentic5,"catch")

LTRM2<-filter(LTRM2,catch>0)
LTRM2<-expandRows (LTRM2, "catch")
 
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



