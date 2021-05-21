library(tidyverse)
library(Rmisc)
library(gsheet)
library(ggplot2)
library(coin)
library(ggpubr)
library(ggbeeswarm)
library(gapminder) 
library(tidyverse) 
library(ggrepel)

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
  dplyr::select(PointBoy, `How much fun were you having in this condition?`, `I felt I was in control of the tricks and the landing.`) %>%
  dplyr::rename(Fun = `How much fun were you having in this condition?`) %>%
  dplyr::rename(Control = `I felt I was in control of the tricks and the landing.`)

WilcoxonData$PointBoy <- factor(WilcoxonData$PointBoy)  

conditionOrderData <- df %>%
  dplyr::group_by(Participant, `Starting Condition`) %>%
  dplyr::summarize(
    AverageControl=mean(`I felt I was in control of the tricks and the landing.`),
    AverageFun=mean(`How much fun were you having in this condition?`)
  )

ParticipantGroup <- df %>%
  dplyr::group_by(Participant, PointBoy, `Starting Condition`) %>%
  dplyr::summarize(
    AverageControl=mean(`I felt I was in control of the tricks and the landing.`),
    AverageFun=mean(`How much fun were you having in this condition?`)
    )

pointBoyYes <- ParticipantGroup %>%
  dplyr::filter(PointBoy=="Yes")

pointBoyNo <- ParticipantGroup %>%
  dplyr::filter(PointBoy=="No")

wilcoxsign_test(pointBoyNo$`AverageFun` ~ pointBoyYes$`AverageFun`, distribution="exact")

t.test(pointBoyNo$`AverageFun`, pointBoyYes$`AverageFun`, paired = TRUE)

wilcoxsign_test(pointBoyNo$`AverageControl` ~ pointBoyYes$`AverageControl`, distribution="exact")

medianFunYes <- median(pointBoyYes$AverageFun, trim=0, na.rm = FALSE)

medianFunNo <- median(pointBoyNo$AverageFun, trim=0, na.rm = FALSE)

medianControlYes <- median(pointBoyNo$AverageControl, trim=0, na.rm = FALSE)

medianControlNo <- median(pointBoyNo$AverageControl, trim=0, na.rm = FALSE)

wilcoxsign_test(pointBoyNo$`Fun` ~ pointBoyYes$`Fun`, distribution="exact")

wilcoxsign_test(pointBoyNo$`Control` ~ pointBoyYes$`Control`, distribution="exact")


ggplot(ParticipantGroup, aes(x = PointBoy, y = AverageFun, group=Participant, label=Participant, color = `Starting Condition`)) + 
  geom_beeswarm(size = 3) +
  geom_line() +
  geom_label_repel(aes(label = paste0("Participant ", Participant)),
                   box.padding   = 0.35, 
                   point.padding = 0.35,
                   segment.color = 'grey50',
                   size = 3) +
  theme_bw()

condition1Start <- conditionOrderData %>%
  dplyr::filter(`Starting Condition` == "pointBoy")

condition2Start <- conditionOrderData %>%
  dplyr::filter(`Starting Condition` == "nopointBoy")

medianFunTotal <- median(df$`How much fun were you having in this condition?`, trim=0, na.rm=FALSE)

medianFun1 <- median(condition1Start$Fun, trim=0, na.rm = FALSE)

medianFun2 <- median(condition2Start$Fun, trim=0, na.rm = FALSE)

medianControl1 <- median(condition1Start$Control, trim=0, na.rm = FALSE)

medianControl2 <- median(condition2Start$Control, trim=0, na.rm = FALSE)

wilcox.test(condition1Start$`AverageFun`, condition2Start$`AverageFun`)

wilcox.test(condition1Start$`AverageControl`, condition2Start$`AverageControl`)
