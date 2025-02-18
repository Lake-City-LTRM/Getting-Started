
# see R tidy text book: https://r4ds.hadley.nz/whole-game

#### install packages ####
# install.packages("tidyverse") # run this line if you need to install tidyverse
# install.packages("styler") # run this line if you need to install styler

#### Load Packages ####
library(tidyverse) # load tidyverse package
#library(ggthemes) # contains colorblind friendly color palettes


#### Load Data ####
fishdata <- read_csv("ltrm_fish_data.csv") # Read In Fish Data
glimpse(fishdata) # take a peek at the data

#### Organize Data ####
# create data table of just lentic fish, as listed in Status and Trends 2022
lentic <- filter(fishdata,fishcode=="LMBS"|fishcode == "BLGL"|fishcode=="BWFN"|
                   fishcode=="WDSN"|fishcode=="NTPK"|fishcode=="BKCP"|
                   fishcode=="LNGR"|fishcode=="WTCP"|fishcode=="YWPH"|
                   fishcode=="GDSN"|fishcode=="STGR"|fishcode=="GNSF"|
                   fishcode=="PNSD"|fishcode=="OSSF")

# create table of annual total lentic fish catch by strata
lentic$sdate<-mdy(lentic$sdate) #convert to date column type
lentic_annualsum <- lentic %>% 
  mutate(year = year(sdate)) %>%  #create year column
  group_by(year,stratum) %>%      # group by year and strata
  summarise(TotCatch = sum(catch))# calculate total catch per strata each year
  
# create table of annual total  fish catch by strata
fishdata$sdate<-mdy(fishdata$sdate) #convert to date column type
fishdata1 <- as.data.frame(fishdata)
fishdata2 <- fishdata1[!(fishdata1$fishcode %in% c(NA)),]
fishdata3 <- fishdata2[!(fishdata2$catch %in% c(NA)),]
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
  summarise(TotCatch = sum(catch))# calculate total catch per strata each year

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

ggplot(data = lentic_annualsum,
       mapping = aes(x=year,y=TotCatch,color = stratum))+
  geom_point()+
  geom_smooth()+
  ggtitle("Total Lentic Fish Catch by Year")+
  theme_bw()

