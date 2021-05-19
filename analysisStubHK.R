library(tidyverse)
library(Rmisc)
library(gsheet)


df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Q4xJHqa2e_rytQor89S_Bk1ZGHItm7OmFkVIkh8WMio/edit#gid=0")
load("dfg.rda")

#remove aggregate material
df<-df[1:32,]

#turn into long format - if you add further columns you need to change the cols (4-11 4:11) accordingly
df <- df %>% pivot_longer(cols = 4:11, names_to = "Measure")
df$Measure <- factor(df$Measure)
df$Measure <- factor(df$Measure)

resbyMeasure <- df %>%
  dplyr::group_by(Measure) %>%
  dplyr::summarize(mean = mean(value))

resbyMeasureByCondition <- df %>%
  dplyr::group_by(Measure, Condition) %>%
  dplyr::summarize(
    mean = mean(value),
    uci = CI(value)[1],
    lci = CI(value)[3]
  )

ggplot(resbyMeasureByCondition, aes(x = Measure, y = mean, colour = Condition)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), position = "dodge", width = 0.25) +
  coord_flip()+theme_bw()
