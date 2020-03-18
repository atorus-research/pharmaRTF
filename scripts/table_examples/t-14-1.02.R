### Table 14-1.02 Summary of End of Study Data

library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)
library(tibble)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

#Read in Source and order factors
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))
adsl$COMPLT24 <- ordered(adsl$COMPLT24, c("Y", "N", NA))
adsl$ARM <- ordered(adsl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))


#### Completion Status Table
comp_stat <- adsl %>%
  group_by(COMPLT24, ARM) %>%
  summarise(n = n())

#Make data.frame for table, unnamed so the cols are named correctly
comp_df <- data.frame(
  "Placebo" = unname(comp_stat[c(1, 4), "n"]),
  "Xanomeline Low Dose" = unname(comp_stat[c(2, 5), "n"]),
  "Xanomeline High Dose" = unname(comp_stat[c(3, 5), "n"]),
  "Total" = c(sum(comp_stat[1:3, "n"]), sum(comp_stat[4:6, "n"])),
  row.names = c("Completed Week 24", "Early Termination (prior to Week 24)")
)

# Add missing row. TODO: probably a more elegant way of doing this.
comp_df["Missing", ] <- 0


#### Reason for Early Termination Table
term_reas <- adsl %>%
  filter(COMPLT24 == "N") %>%
  group_by(DSDECOD, ARM) %>%
  summarise(n = n())
