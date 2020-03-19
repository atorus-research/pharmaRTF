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
adsl$DSDECOD <- ordered(adsl$DSDECOD, c("ADVERSE EVENT", "DEATH", "LACK OF EFFICACY, PATIENT CAREGIVER PERCEPTION",
                                        "UNABLE TO CONTACT PATIENT (LOST TO FOLLOW-UP)",
                                        "PERSONAL CONFLICT OR OTHER PATIENT/CAREGIVER DECISION",
                                        "PHYSICIAN DECISION", "PROTOCOL ENTRY CRITERIA NOT MET",
                                        "PROTOCOL VIOLATION",
                                        "SPONSOR DECISION (STUDY OR PATIENT DISCONTINUED BY THE SPONSOR)"))


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

# p-value
comp_p <- fish_p(adsl, adsl$COMPLT24, adsl$ARM)
comp_df <- attach_p(comp_df, comp_p)

#### Reason for Early Termination Table
## By ARM
term_reas <- adsl %>%
  filter(COMPLT24 == "N") %>%
  group_by(DSDECOD, ARM) %>%
  complete(nesting(DSDECOD, ARM)) %>%
  summarise(n = n())

## Total
term_reas_tot <- adsl %>%
  filter(COMPLT24 == "N") %>%
  group_by(DSDECOD) %>%
  complete(nesting(DSDECOD, ARM)) %>%
  summarise(n = n())

term_df <- data.frame(
  "Placebo" = unname(term_reas[seq(1, 27, 3), "n"]),
  "Xanomeline Low Dose" = unname(term_reas[seq(2, 27, 3), "n"]),
  "Xanomeline High Dose" = unname(term_reas[seq(3, 27, 3),"n"]),
  "Total" = unname(term_reas_tot[,"n"]),
  row.names = c(
    "Adverse Event",
    "Death",
    "Lack of Efficacy[2]",
    "Lost to Follow-up",
    "Subject decided to withdraw",
    "Physician decided to withdraw subject",
    "Protocol criteria not met",
    "Protocol violation",
    "Sponsor decision"
  )
)
term_df["Missing", ] <- 0

# p-value
term_p_1 <- adsl %>%
  fish_p(DSREASAE, ARM)
term_df <- attach_p(term_df, term_p_1)
### TODO: FIgure out the p value derivation on 'Lack of Efficacy'

## Add Table lables
comp_df <- add_column(comp_df, " " = row.names(comp_df), .before = 1)
comp_df <- add_row(comp_df, " " = "Completion Status:", .before = 1)
comp_df <- add_row(comp_df, " " = "", .before = 1)

term_df <- add_column(term_df, " " = row.names(term_df), .before = 1)
term_df <- add_row(term_df, " " = "Reason for Early Termination (prior to Week 24):", .before = 1)
term_df <- add_row(term_df, " " = "", .before = 1)






