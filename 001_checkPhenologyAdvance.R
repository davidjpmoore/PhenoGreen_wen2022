library(tidyr)
library(dplyr)
library(ggplot2)

LAI_SpringAdvancewide=read.csv(file="data/SpringAdvanceOnly.csv",header = TRUE)

LAISpringAdvance_slim <-
  pivot_longer(data= LAI_SpringAdvancewide,
               cols = starts_with("LAI"),
               names_to = "YEARind",
               values_to = "LAI"
  )

LAISpringAdvance_slim$YEAR= as.numeric(substr(LAISpringAdvance_slim$YEARind, start = 4, stop = 7))
LAISpringAdvance_slim$SEASON="none"
LAISpringAdvance_slim$SEASON[LAISpringAdvance_slim$day>90]="early"
LAISpringAdvance_slim$SEASON[LAISpringAdvance_slim$day>150]="mid"
LAISpringAdvance_slim$SEASON[LAISpringAdvance_slim$day>200]="late"
LAISpringAdvance_slim$SEASON[LAISpringAdvance_slim$day>250]="none"

plot(LAISpringAdvance_slim$YEAR,LAISpringAdvance_slim$LAI)

library(dplyr)

TrendLAI = LAISpringAdvance_slim %>%
  group_by(YEAR,SEASON) %>%
  summarise_at(vars(LAI), list(LAImean = mean))

library(ggplot2)

p <- ggplot(TrendLAI, aes(YEAR, LAImean))
# A basic scatter plot
p + geom_point(aes(colour = factor(SEASON)), size = 4)


# GROWTH STIMULATION ONLY 

StimOnlywide=read.csv(file="data/GrowthStimOnly.csv",header = TRUE)

StimOnly_slim <-
  pivot_longer(data= StimOnlywide,
               cols = starts_with("LAI"),
               names_to = "YEARind",
               values_to = "LAI"
  )

StimOnly_slim$YEAR= as.numeric(substr(StimOnly_slim$YEARind, start = 4, stop = 7))
StimOnly_slim$SEASON="none"
StimOnly_slim$SEASON[StimOnly_slim$day>90]="early"
StimOnly_slim$SEASON[StimOnly_slim$day>150]="mid"
StimOnly_slim$SEASON[StimOnly_slim$day>200]="late"
StimOnly_slim$SEASON[StimOnly_slim$day>250]="none"

plot(StimOnly_slim$YEAR,StimOnly_slim$LAI)

bs <- ggplot(StimOnly_slim, aes(day, LAI))
bs + geom_point(aes(colour = factor(YEAR)), size = 4)

TrendLAI = StimOnly_slim %>%
  group_by(YEAR,SEASON) %>%
  summarise_at(vars(LAI), list(LAImean = mean))


p <- ggplot(TrendLAI, aes(YEAR, LAImean))
# A basic scatter plot
p + geom_point(aes(colour = factor(SEASON)), size = 4)


#Advanced spring and growth stimulation
Advance_and_GrStim=read.csv(file="data/Advance_and_GrStim.csv",header = TRUE)

Adv_GrStim_slim <-
  pivot_longer(data= Advance_and_GrStim,
               cols = starts_with("LAI"),
               names_to = "YEARind",
               values_to = "LAI"
  )

Adv_GrStim_slim$YEAR= as.numeric(substr(Adv_GrStim_slim$YEARind, start = 4, stop = 7))
Adv_GrStim_slim$SEASON="none"
Adv_GrStim_slim$SEASON[Adv_GrStim_slim$day>90]="early"
Adv_GrStim_slim$SEASON[Adv_GrStim_slim$day>150]="mid"
Adv_GrStim_slim$SEASON[Adv_GrStim_slim$day>200]="late"
Adv_GrStim_slim$SEASON[Adv_GrStim_slim$day>250]="none"

plot(Adv_GrStim_slim$YEAR,Adv_GrStim_slim$LAI)

agTS <- ggplot(Adv_GrStim_slim, aes(day, LAI))
agTS + geom_point(aes(colour = factor(YEAR)), size = 4)

TrendLAI = Adv_GrStim_slim %>%
  group_by(YEAR,SEASON) %>%
  summarise_at(vars(LAI), list(LAImean = mean))


ag <- ggplot(TrendLAI, aes(YEAR, LAImean))
# A basic scatter plot
ag + geom_point(aes(colour = factor(SEASON)), size = 4)


