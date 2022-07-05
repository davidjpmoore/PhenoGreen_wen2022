library(tidyr)
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

