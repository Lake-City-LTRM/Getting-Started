getwd()
setwd("C:/Users/kbouska/OneDrive - DOI/Documents/LTRM Data/regime_shifts")

library(splitstackshape)

#load ltrm data
LTRM =read.csv(file="ltrm_fish_data.csv", header=TRUE, sep=",")
attach(LTRM)
str(LTRM)
#remove fish with weight <=0 (1/5 of data)
LTRM_bm<-LTRM [which(weight>0), ]
#subset master databse variables
LTRM_bm2 <- LTRM_bm[,c(3, 6, 73, 74, 78)]

#add log length
LTRM_bm2$loglength <- log10( LTRM_bm2$length)
#add log weight
LTRM_bm2$logweight <- log10( LTRM_bm2$weight)

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

#estimate ordinary least square regression coefficients for CNCF - improved 
# estimates - is it worth updating the life history database?
LTRM_CNCF<-LTRM_bm2 [which(LTRM_bm2$fishcode=="CNCF"), ]
lmodel <- lm(logweight ~ loglength, data = LTRM_CNCF)
lmodel$coefficients

plot(LTRM_CNCF$logweight~LTRM_CNCF$loglength)
plot(LTRM_CNCF$weight~LTRM_CNCF$length)


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
LW =read.delim2(file="lifehistory_growth.txt", header=TRUE, sep=",")
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
LTRM4$sdate <-as.Date(LTRM4$sdate, '%m/%d/%Y')
LTRM4$year<-format(LTRM4$sdate,"%Y")
write.csv(LTRM4, "LTRM4.csv")

#summarize by barcode - total biomass by species (mpue)
LTRM_bm_sum<- LTRM4 %>%
  group_by(barcode, fishcode) %>%
  summarise (bm_sum = sum(mpue, na.rm = TRUE))



#add sample information back in
LTRM_sampleinfo <- LTRM4[,c(1,2,3,4)]
LTRM_sampleinfo2<-unique(LTRM_sampleinfo)
LTRM_bm_sum2<-inner_join(LTRM_bm_sum, LTRM_sampleinfo2, by = c("barcode" = "barcode"))
LTRM_bm_sum2$sdate <-as.Date(LTRM_bm_sum2$sdate, '%m/%d/%Y')
LTRM_bm_sum2$year<-format(LTRM_bm_sum2$sdate,"%Y")

#remove non SRS strata
LTRM_bm_srs <- LTRM_bm_sum2[ which(LTRM_bm_sum2$stratum=='MCB-U' |
                                     LTRM_bm_sum2$stratum=='SCB' |
                                     LTRM_bm_sum2$stratum=='BWC-S' |
                                     LTRM_bm_sum2$stratum=='IMP-S' ), ]

#sum mpue by stata, fstation, year (to calculate mean mpue per strata)
LTRM_bm_srs_strata<-LTRM_bm_srs %>%
  group_by(stratum, year, fstation, fishcode) %>%
  summarize(sum.mpue = sum(bm_sum, na.rm = TRUE))

#summarize # of barcodes per reach, year, stratum combination (to calculate mean mpue per strata)
barcodes<- LTRM_bm_srs %>%                    # take the data.frame "data"
  group_by(stratum, year, fstation) %>%          # Then, with the filtered data, group it by "bb"
  summarise(Unique_barcode = n_distinct(barcode))   # Now summarise with unique elements per group

#join with srs_strata table
LTRM_bm_srs_strata_mean<-inner_join(LTRM_bm_srs_strata, barcodes, by = c("stratum" = "stratum", "fstation"="fstation", "year"="year" ))
#calculate mean mpue per strata (yh)
LTRM_bm_srs_strata_mean$mpue<-(LTRM_bm_srs_strata_mean$sum.mpue/LTRM_bm_srs_strata_mean$Unique_barcode)

#load population data
pop =read.csv(file="fish_srs_pop.csv", header=TRUE, sep=",")
LTRM_bm_srs_strata_pop<-inner_join(LTRM_bm_srs_strata_mean, pop, by = c("stratum" = "stratum", "fstation"="fstation" ))
#multiply mpue *Nh
LTRM_bm_srs_strata_pop$mpueXNh <- LTRM_bm_srs_strata_pop$mpue * LTRM_bm_srs_strata_pop$N

#sum mpueXNh across stratum per fishcode, fstation, year
LTRM_bm_srs_poolwide<-LTRM_bm_srs_strata_pop %>%
  group_by( year, fstation, fishcode) %>%
  summarize(sum.mpueXNh = sum(mpueXNh, na.rm = TRUE))
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

#evaluate with treemap - looks very close (not 100%)
LTRM_CARP<-LTRM_bm_srs_poolwide [which(LTRM_bm_srs_poolwide$fishcode=="CARP" & LTRM_bm_srs_poolwide$fstation==1), ]
plot(LTRM_CARP$pw.mpue~LTRM_CARP$year)
LTRM_BLGL<-LTRM_bm_srs_poolwide [which(LTRM_bm_srs_poolwide$fishcode=="BLGL" & LTRM_bm_srs_poolwide$fstation==6), ]
plot(LTRM_BLGL$pw.mpue~LTRM_BLGL$year)
LTRM_LMBS<-LTRM_bm_srs_poolwide [which(LTRM_bm_srs_poolwide$fishcode=="LMBS" & LTRM_bm_srs_poolwide$fstation==6), ]
plot(LTRM_LMBS$pw.mpue~LTRM_LMBS$year)
LTRM_SVCP<-LTRM_bm_srs_poolwide [which(LTRM_bm_srs_poolwide$fishcode=="SVCP" & LTRM_bm_srs_poolwide$fstation==6), ]
plot(LTRM_SVCP$pw.mpue~LTRM_SVCP$year)

#calculate variance 
#inner joing to add yh, yhi and nh to LTRM_bm_srs to calculate s2h
LTRM_bm_srs_variance<-inner_join(LTRM_bm_srs, LTRM_bm_srs_strata_mean, by = c("fishcode" = "fishcode", "fstation"="fstation", "year"="year", "stratum"="stratum"))
#calculate (yhi - yh)
LTRM_bm_srs_variance$resid <- LTRM_bm_srs_variance$bm_sum - LTRM_bm_srs_variance$mpue
LTRM_bm_srs_variance$residp2 <-LTRM_bm_srs_variance$resid*LTRM_bm_srs_variance$resid
LTRM_bm_srs_variance$sampvar <- (LTRM_bm_srs_variance$residp2 / (LTRM_bm_srs_variance$Unique_barcode-1))

#calculate stratum variance from sample variance
LTRM_bm_srs_strata_var<-LTRM_bm_srs_variance %>%
  group_by( year, fstation, fishcode, stratum) %>%
  summarize(stravar = sum(sampvar, na.rm = TRUE))
#join in nh and Nh from LTRM_bm_srs_strata_mean
LTRM_bm_srs_var<-inner_join(LTRM_bm_srs_strata_var, LTRM_bm_srs_strata_pop, by = c("fishcode" = "fishcode", "fstation"="fstation", "year"="year", "stratum"="stratum"))
LTRM_bm_srs_var$s2_n <-LTRM_bm_srs_var$stravar/LTRM_bm_srs_var$Unique_barcode
LTRM_bm_srs_var$Nh_nh <-LTRM_bm_srs_var$N - LTRM_bm_srs_var$Unique_barcode
LTRM_bm_srs_var$varprod <- (LTRM_bm_srs_var$N *LTRM_bm_srs_var$Nh_nh *LTRM_bm_srs_var$s2_n)
#calculate sum of varprod
LTRM_bm_srs_pw_var<-LTRM_bm_srs_var %>%
  group_by( year, fstation, fishcode) %>%
  summarize(sum.varprod = sum(varprod, na.rm = TRUE))
#poolwide variance and sd calculation
LTRM_bm_srs_pw_var_N<-inner_join(LTRM_bm_srs_pw_var, LTRM_bm_srs_poolwide, by = c("fishcode" = "fishcode", "fstation"="fstation", "year"="year"))
LTRM_bm_srs_pw_var_N$pw.var<-(LTRM_bm_srs_pw_var_N$sum.varprod/(LTRM_bm_srs_pw_var_N$N^2))
LTRM_bm_srs_pw_var_N$sd<-(sqrt(LTRM_bm_srs_pw_var_N$pw.var))
LTRM_bm_srs_pw_var_N$cv<-(LTRM_bm_srs_pw_var_N$sd/LTRM_bm_srs_pw_var_N$pw.mpue)
write.csv(LTRM_bm_srs_pw_var_N, file = "LTRM_bm_srs_poolwide.csv")

LTRM_bm_srs_pw_var_N_24<-LTRM_bm_srs_pw_var_N |>
  filter(fishcode==("BLGL") | fishcode==("SVCP") |fishcode==("BHCP"))
write.csv(LTRM_bm_srs_pw_var_N_24, file = "LTRM_bm_srs_poolwide_blgl_ms.csv")

#add in trophic guild and body size grouping information (rely on Bouska 2018 paper)
Trophic =read.csv(file="Trophic_scale.csv", header=TRUE, sep=",")
LTRM_bm_srs_poolwide_tg<-inner_join(LTRM_bm_srs_poolwide, Trophic, by = c("fishcode" = "fishcode"))
LTRM_bm_srs_poolwide_tg_var<-inner_join(LTRM_bm_srs_pw_var_N, Trophic, by = c("fishcode" = "fishcode"))
write.csv(LTRM_bm_srs_poolwide_tg_var, file = "LTRM_bm_srs_poolwide_tg_var.csv")
write.csv(LTRM_bm_srs_poolwide_tg, file = "LTRM_bm_srs_poolwide_tg.csv")


#sum poolwide mpue by trophic guild per fstation & year & size calss
LTRM_bm_srs_pw_tg_sum<-LTRM_bm_srs_poolwide_tg %>%
  group_by(fstation, year, Size.aggregation.class..reaches.4.26., Trophic.level) %>%
  summarize(sum.mpue = sum(pw.mpue, na.rm = TRUE))
#write.csv(LTRM_bm_srs_pw_tg_sum, file = "LTRM_bm_srs_pw_tg_sum.csv")

LTRM_bm_srs_pw_tg_sum_var<-LTRM_bm_srs_poolwide_tg_var %>%
  group_by(fstation, year, Size.aggregation.class..reaches.4.26., Trophic.level) %>%
  summarize(sum.var = sum(pw.var, na.rm = TRUE))
LTRM_bm_srs_pw_tg_sum_var$tg_sd<-(sqrt(LTRM_bm_srs_pw_tg_sum_var$sum.var))
LTRM_bm_srs_pw_tg_sum_var$tg_cv<-(LTRM_bm_srs_pw_tg_sum_var$tg_sd/LTRM_bm_srs_pw_tg_sum$sum.mpue)
write.csv(LTRM_bm_srs_pw_tg_sum_var, file = "LTRM_bm_srs_pw_tg_sum_var.csv")

#sum poolwide mpue per fstation & year & size class
LTRM_bm_srs_pw_sum<-LTRM_bm_srs_poolwide_tg %>%
  group_by(fstation, year) %>%
  summarize(sum.mpue = sum(pw.mpue, na.rm = TRUE))
write.csv(LTRM_bm_srs_pw_sum, file = "LTRM_bm_srs_pw_sum.csv")

#pool-wide mpue and var without SVCP, BHCP and CARP
LTRM_bm_srs_poolwide_tg_noinv <- subset(LTRM_bm_srs_poolwide_tg, fishcode!="SVCP")
LTRM_bm_srs_poolwide_tg_noinv <- subset(LTRM_bm_srs_poolwide_tg_noinv, fishcode!="CARP")
LTRM_bm_srs_poolwide_tg_noinv <- subset(LTRM_bm_srs_poolwide_tg_noinv, fishcode!="BHCP")
LTRM_bm_srs_poolwide_tg_var_noinv <-subset(LTRM_bm_srs_poolwide_tg_var, fishcode!="SVCP")
LTRM_bm_srs_poolwide_tg_var_noinv <- subset(LTRM_bm_srs_poolwide_tg_var_noinv, fishcode!="CARP")
LTRM_bm_srs_poolwide_tg_var_noinv <- subset(LTRM_bm_srs_poolwide_tg_var_noinv, fishcode!="BHCP")
write.csv(LTRM_bm_srs_poolwide_tg_noinv, file = "LTRM_bm_srs_poolwide_tg_noinv.csv")

LTRM_bm_srs_pw_tg_sum_var_no_inv<-LTRM_bm_srs_poolwide_tg_noinv %>%
  group_by(fstation, year, Size.aggregation.class..reaches.4.26., Trophic.level) %>%
  summarize(sum.mpue = sum(pw.mpue, na.rm = TRUE))
#write.csv(LTRM_bm_srs_pw_tg_sum_var_no_inv, file = "LTRM_bm_srs_pw_tg_sum_var_no_inv.csv")

LTRM_bm_srs_pw_tg_sum_var_no_inv<-LTRM_bm_srs_poolwide_tg_var_noinv %>%
  group_by(fstation, year, Size.aggregation.class..reaches.4.26., Trophic.level) %>%
  summarize(sum.var = sum(pw.var, na.rm = TRUE))
LTRM_bm_srs_pw_tg_sum_var_no_inv$tg_sd<-(sqrt(LTRM_bm_srs_pw_tg_sum_var_no_inv$sum.var))









