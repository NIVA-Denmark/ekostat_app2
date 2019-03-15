library(tidyverse)
#library(purrr)

load("resAvg.Rda")
load("resMC.Rda")
source("Aggregation.R")

res1Avg <-
  Aggregate(
    resAvg,
    Groups = c("Period"),
    level = 1
  ) 

res1MC <- AggregateMCovr(resMC,resAvg,res1Avg)

res1MC <-
  Aggregate(
    resMC,
    Groups = c("Period", "sim"),
    level = 1
  ) %>% rename(ClassMC = Class)

values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Period"))
