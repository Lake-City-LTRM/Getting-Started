
# see R tidy text book: https://r4ds.hadley.nz/whole-game

#### install packages ####
# install.packages("tidyverse") # run this line if you need to install tidyverse
# install.packages("styler") # run this line if you need to install styler

#### Load Packages ####
library(tidyverse) # load tidyverse package
#library(ggthemes) # contains colorblind friendly color palettes


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

lentic4<-lentic3 %>% 
  filter(stratum=="MCB-U"|stratum=="SCB"|stratum=="BWC-S") %>% 
  mutate(stratum = if_else(stratum=="SCB","SC",stratum)) %>% 
  filter(gear=="D") %>%  ## only day electrofishing
  mutate(stratum = if_else(stratum =="MCB-U"& gear=="D","MCB-S",stratum)) %>%  # from https://www.umesc.usgs.gov/ltrmp/stats/code/sampling_weights_BLGL_day.sas
  select(sdate,stratum,effmin,fishcode,catch, weight) %>% 
  mutate(mass = as.numeric(weight)) %>% 
  filter(!is.na(mass)) %>% 
  mutate(MPUE = mass/effmin*15)

lentic_annualsum <- lentic4 %>% 
  mutate(year = year(sdate)) %>%  #create year column
  group_by(year,stratum) %>%      # group by year and strata
  summarise(AnnMeanMPUE = sum(MPUE),Asd = sd(MPUE))# calculate total catch per strata each year
  
### LENTIC FISH PLOT RECREATION FIGURE G3 from S&T
ggplot(data = lentic_annualsum,
       mapping = aes(x=year,y=AnnMeanMPUE,color = stratum))+
  geom_point()+
 # geom_errorbar(aes(ymin=(AnnMeanMPUE-Asd),ymax=(AnnMeanMPUE+Asd)))+
  geom_smooth(method = "lm")+
  ggtitle("Annual Average MPUE (15 min e) by Year and Stratum")+
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



