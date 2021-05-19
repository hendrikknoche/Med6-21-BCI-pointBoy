library(tidyverse)
library(Rmisc)
library(gsheet)
library(ggplot2)
library(coin)


df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Q4xJHqa2e_rytQor89S_Bk1ZGHItm7OmFkVIkh8WMio/edit#gid=33771305")
load("dfg.rda")

#turn into long format - if you add further columns you need to change the cols (4-11 4:11) accordingly
df2 <- df %>% 
      pivot_longer(cols = 5:12, names_to = "Measure")

df2$Measure <- factor(df2$Measure)

resbyMeasure <- df2 %>%
  dplyr::group_by(Measure) %>%
  dplyr::summarize(mean = mean(value,na.rm=TRUE))

resbyMeasureByCondition <- df2 %>%
  dplyr::group_by(Measure, Condition) %>%
  dplyr::summarize(
    mean = mean(value),
    uci = CI(value)[1],
    lci = CI(value)[3]
  )

WilcoxonData <- df %>%
  dplyr::select(PointBoy, `How much fun were you having in this condition?`) %>%
  dplyr::rename(Fun = `How much fun were you having in this condition?`)

pointBoyYes <- WilcoxonData %>%
  dplyr::filter(PointBoy=="Yes")

pointBoyNo <- WilcoxonData %>%
  dplyr::filter(PointBoy=="No")

wilcoxsign_test(pointBoyNo$`How much fun were you having in this condition?` ~ pointBoyYes$`How much fun were you having in this condition?`, distribution="exact")

ggplot(resbyMeasureByCondition, aes(x = Measure, y = mean, colour = Condition)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), position = "dodge", width = 0.25) +
  coord_flip()+theme_bw()

