### Table 14-7.01: Summary of Concomitant Medications (Number of Subjects)

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

cm <- read_xpt(glue("{sdtm_lib}/cm.xpt"))
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))
adsl$ARM <- ordered(adsl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
### TODO: add ordering for medication names because they aren't alphabetical for some reason

## Patients receiving at least one medication
cm_1 <- adsl %>%
  group_by(ARM) %>%
  summarise(n = sum(USUBJID %in% unique(cm$USUBJID)),
            total = n())
n_pct(cm_1$n, cm_1$total)


### Table
# Medication classes
cm_class <- sort(unique(cm$CMCLAS))

# By Class
temp <- ldply(cm_class, function(class_i){
  class_by_arm <- adsl %>%
    group_by(ARM) %>%
    summarise(n = sum(USUBJID %in% unlist(unique(cm[cm$CMCLAS == class_i, "USUBJID"]))))
  df_1 <- data.frame(
    "Therapeutic class, n (%)" = class_i,
    "Placebo" = unname(class_by_arm[1, "n"]),
    "Xanomeline Low Dose" = unname(class_by_arm[2, "n"]),
    "Xanomeline High Dose" = unname(class_by_arm[3, "n"]),
    stringsAsFactors = FALSE, check.names = FALSE, row.names = FALSE
  )

  #Coded medication names
  cm_medi <- unlist(unique(cm[cm$CMCLAS == class_i, "CMDECOD"]), use.names = FALSE)

  #By Medication
  df_2 <- ldply(cm_medi, function(medi_i) {
    medi_by_arm <- adsl %>%
      group_by(ARM) %>%
      summarise(n = sum(USUBJID %in% unlist(unique(cm[cm$CMDECOD == medi_i, "USUBJID"]))))
    data.frame(
      "Therapeutic class, n (%)" = paste0("\t", unname(medi_i)),
      "Placebo" = unname(medi_by_arm[1, "n"]),
      "Xanomeline Low Dose" = unname(medi_by_arm[2, "n"]),
      "Xanomeline High Dose" = unname(medi_by_arm[3, "n"]),
      stringsAsFactors = FALSE, check.names = FALSE, row.names = FALSE
    )
  })
  rbind(df_1, df_2)
})







