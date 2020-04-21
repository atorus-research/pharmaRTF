# T-14-6.04


library(huxtable)
library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(pharmaRTF)
library(tibble)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

## Chem
adlbc <- read_xpt(glue("{adam_lib}/adlbc.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
comb <- rbind(adlbc, adlbh)

#sort tests
comb$LBTEST <-ordered(comb$LBTEST, c(sort(unique(adlbc$LBTEST)), sort(unique(adlbh$LBTEST))))
comb$TRTP <- ordered(comb$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
comb$BLTRFL <- ordered(comb$BLTRFL, c("N", "H"))
comb$LBTRFL <- ordered(comb$LBTRFL, c("N", "H"))
comb$VISIT <- ordered(comb$VISIT, c(
  "WEEK 2",
  "WEEK 4",
  "WEEK 6",
  "WEEK 8",
  "WEEK 12",
  "WEEK 16",
  "WEEK 20",
  "WEEK 24",
  "WEEK 26"
))

comb2 <- comb %>%
  filter(!is.na(VISIT), !is.na(TRTP), !is.na(BLTRFL), !is.na(LBTRFL)) %>%
  group_by(LBTEST, VISIT, TRTP, BLTRFL, LBTRFL) %>%
  complete(nesting(BLTRFL, LBTRFL)) %>%
  summarise(N = n()) %>%
  pivot_wider(id_cols = c(LBTEST, VISIT, LBTRFL), names_from = c(TRTP, BLTRFL), values_from = N)


