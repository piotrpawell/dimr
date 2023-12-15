#### commands for DIMR analysis
## required packages
library(tidyverse)
library(haven)
library(ggstatsplot)
library(ggplot2)
library(lme4)
library(lmerTest)

#### load the data
#### repository for dimr_data and two aggregated sets https://github.com/piotrpawell/dimr
# dimr_data.Rdata is the file database with all observations
# dimr_a01.Rdata is database for first analysis (it is filtered data by collaborative projects and WaveYears)
# dimr_a02.Rdata is database for second analysis (it is filtered data by collaborative projects and WaveYears)


a <- load("dimr_data.RData")

# Analysis 1: The years for which the ESS, ISSP, and EVS/WVS measurement all took place: from 2008 to 2010 and from 2017 to 2019 and countries in which Christianity is the predominant religion

# cc=Countrycombined1
# pm=PROJECT_MAIN
# bp=BELONGING_COMBINED
# pm3 recoded pm to EVS/WVS, ISSP, ESS

# dimr_a01.Rdata is database for first analysis
a_cc28<- a %>%
  filter(((WaveYear>=2008 & WaveYear<=2010) | WaveYear>=2017 & WaveYear<=2019)
         & bp=="Christianity" &
           (cc=="Austria" |
           cc=="Belgium" |
           cc=="Bulgaria" |
           cc=="Croatia" |
           cc=="Cyprus" |
           cc=="Czech Republic" |
           cc=="Denmark" |
           cc=="Finland" |
           cc=="France" |
           cc=="Germany" |
           cc=="Hungary" |
           cc=="Iceland" |
           cc=="Ireland" |
           cc=="Italy" |
           cc=="Latvia" |
           cc=="Lithuania" |
           cc=="Netherlands" |
           cc=="Norway" |
           cc=="Poland" |
           cc=="Portugal" |
           cc=="Russian Federation" |
           cc=="Slovak Republic" |
           cc=="Slovenia" |
           cc=="Spain" |
           cc=="Sweden" |
           cc=="Switzerland" |
           cc=="Ukraine" |
           cc=="Great Britan")
  )


# agregated data for 3 variables IMP_SDR, IMP_WPR and IMP_ATT
att_sdr <- aggregate(SDR_01F ~ pm3 + WaveYear + cc + cohort10, 
                     data = a_cc28, FUN = mean)
att_pra <- aggregate(pray52 ~ pm3 + WaveYear + cc + cohort10, 
                     data = a_cc28, FUN = mean)
att_atn <- aggregate(rlgatnd52 ~ pm3 + WaveYear + cc + cohort10, 
                     data = a_cc28, FUN = mean)

# linear mixed models
# for IMP_SDR
sdr01<-lmer(formula = SDR_01F ~ 1 +(1| cc) +(1| pm3),
            data = att_sdr, REML=F)
sdr02<-lmer(formula = SDR_01F ~ 1 + cohort10 +(1| cc) +(1| pm3),
            data = att_sdr, REML=F)
sdr02<-lmer(formula = SDR_01F ~ 1 + cohort10 +(1| cc) +(1| pm3) + (1| WaveYear),
            data = att_sdr, REML=F)
# for IMP_WPR
pra01<-lmer(formula = pray52 ~ 1 +(1| cc) +(1| pm3),
            data = att_pra, REML=F)
pra02<-lmer(formula = pray52 ~ 1 + cohort10 +(1| cc) +(1| pm3),
            data = att_pra, REML=F)
pra03<-lmer(formula = pray52 ~ 1 + cohort10 +(1| cc) +(1| pm3) + (1| WaveYear),
            data = att_pra)
# for IMP_ATT
atn01<-lmer(formula = rlgatnd52 ~ 1 +(1| cc) +(1| pm3),
            data = att_atn, REML=F)
atn02<-lmer(formula = rlgatnd52 ~ 1 + cohort10 +(1| cc) +(1| pm3),
            data = att_atn, REML=F)
atn03<-lmer(formula = rlgatnd52 ~ 1 + cohort10 +(1| cc) +(1| pm3)+ (1| WaveYear),
            data = att_atn, REML=F)



# Analysis 2: The second subset for analysis is the years 1990-1993, 1998-2000, 2007-2010 and 2017-2020 contains data only for EVS/WVS and ISSP and countries in which Christianity is the predominant religion

# dimr_a02.Rdata is database for second analysis
a_issp<- a %>%
  filter((WaveYear>=1990 & WaveYear<=1993 |
            WaveYear>=1998 & WaveYear<=2000 |
            WaveYear>=2007 & WaveYear<=2010 |
            WaveYear>=2017 & WaveYear<=2020
  )
  & bp=="Christianity" & pm3!="ESS" & (
      cc=="Australia" |
      cc=="Austria" |
      cc=="Belgium" |
      cc=="Bulgaria" |
      cc=="Canada" |
      cc=="Chile" |
      cc=="Croatia" |
      cc=="Cyprus" |
      cc=="Czech Republic" |
      cc=="Denmark" |
      cc=="Finland" |
      cc=="France" |
      cc=="Georgia" |
      cc=="Germany" |
      cc=="Hungary" |
      cc=="Iceland" |
      cc=="Ireland" |
      cc=="Italy" |
      cc=="Korea (South)" |
      cc=="Latvia" |
      cc=="Mexico" |
      cc=="Netherlands" |
      cc=="New Zealand" |
      cc=="Norway" |
      cc=="Philippines" |
      cc=="Poland" |
      cc=="Portugal" |
      cc=="Russian Federation" |
      cc=="Slovak Republic" |
      cc=="Slovenia" |
      cc=="Spain" |
      cc=="Sweden" |
      cc=="Switzerland" |
      cc=="Ukraine" |
      cc=="Great Britan" |
      cc=="USA" |
      cc=="Northern Ireland"))

# agregated data for 3 variables IMP_SDR, IMP_WPR and IMP_ATT
issp_sdr <- aggregate(SDR_01F ~ pm3 + WaveYear + cc + cohort10, 
                      data = a_isspf, FUN = mean)
issp_pray <- aggregate(pray52 ~ pm3 + WaveYear + cc + cohort10, 
                       data = a_isspf, FUN = mean)
issp_atnd <- aggregate(rlgatnd52 ~ pm3 + WaveYear + cc + cohort10, 
                       data = a_isspf, FUN = mean)

# linear mixed models
# for IMP_SDR
sdri01<-lmer(formula = SDR_01F ~ 1 +(1| cc)+(1| pm3),
             data = issp_sdr, REML=F)
sdri02<-lmer(formula = SDR_01F ~ 1 + cohort10 + (1| cc)+(1| pm3),
             data = issp_sdr, REML=F)
sdri03<-lmer(formula = SDR_01F ~ 1 + cohort10 + (1| cc)+(1| pm3) + (1| WaveYear),
                data = issp_sdr, REML=F)
# for IMP_WPR
prais01<-lmer(formula = pray52 ~ 1 +(1| cc)+(1|pm3),
              data = issp_pray, REML=F)
prais02<-lmer(formula = pray52 ~ 1 + cohort10 + (1| cc)+(1| pm3),
              data = issp_pray, REML=F)
prais03<-lmer(formula = pray52 ~ 1 + cohort10 + (1| cc)+(1| pm3) + (1| WaveYear),
                 data = issp_pray, REML=F)
# for IMP_ATT
atnd01<-lmer(formula = rlgatnd52 ~ 1 +(1| cc) +(1| pm3),
            data = issp_atnd, REML=F)
atnd02<-lmer(formula = rlgatnd52 ~ 1 + cohort10 +(1| cc) +(1| pm3),
            data = issp_atnd, REML=F)
atnd03<-lmer(formula = rlgatnd52 ~ 1 + cohort10 + (1| cc)+(1| pm3) + (1| WaveYear),
                data = issp_atnd, REML=F)

