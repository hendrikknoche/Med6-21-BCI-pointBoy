library(tidyverse)
library(Rmisc)
library(gsheet)
library(ggplot2)
library(coin)
library(ggpubr)

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

conditionOrderData <- df %>%
  dplyr::select(`Starting Condition`, `How much fun were you having in this condition?`, `I felt I was in control of the tricks and the landing.`) %>%
  dplyr::rename(Fun = `How much fun were you having in this condition?`) %>%
  dplyr::rename(Control = `I felt I was in control of the tricks and the landing.`)


pointBoyYes <- WilcoxonData %>%
  dplyr::filter(PointBoy=="Yes")

pointBoyNo <- WilcoxonData %>%
  dplyr::filter(PointBoy=="No")

medianFunYes <- median(pointBoyYes$Fun, trim=0, na.rm = FALSE)

medianFunNo <- median(pointBoyNo$Fun, trim=0, na.rm = FALSE)

medianControlYes <- median(pointBoyYes$Control, trim=0, na.rm = FALSE)

medianControlNo <- median(pointBoyNo$Control, trim=0, na.rm = FALSE)

wilcoxsign_test(pointBoyNo$`Fun` ~ pointBoyYes$`Fun`, distribution="exact")

wilcoxsign_test(pointBoyNo$`Control` ~ pointBoyYes$`Control`, distribution="exact")

ggplot(WilcoxonData, aes(x = PointBoy, y = Fun, fill = PointBoy)) + 
  geom_boxplot()

ggplot(WilcoxonData, aes(x = PointBoy, y = Control, fill = PointBoy)) + 
  geom_boxplot()

condition1Start <- conditionOrderData %>%
  dplyr::filter(`Starting Condition` == "pointBoy")

condition2Start <- conditionOrderData %>%
  dplyr::filter(`Starting Condition` == "nopointBoy")

medianFunTotal <- median(df$`How much fun were you having in this condition?`, trim=0, na.rm=FALSE)

medianFun1 <- median(condition1Start$Fun, trim=0, na.rm = FALSE)

medianFun2 <- median(condition2Start$Fun, trim=0, na.rm = FALSE)

medianControl1 <- median(condition1Start$Control, trim=0, na.rm = FALSE)

medianControl2 <- median(condition2Start$Control, trim=0, na.rm = FALSE)

wilcox.test(condition1Start$`Fun`, condition2Start$`Fun`)

wilcox.test(condition1Start$`Control`, condition2Start$`Control`)

ggplot(conditionOrderData, aes(x = `Starting Condition`, y = Fun, fill = `Starting Condition`)) + 
  geom_boxplot()

ggplot(conditionOrderData, aes(x = `Starting Condition`, y = Control, fill = `Starting Condition`)) + 
  geom_boxplot()
