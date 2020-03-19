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

## Patients receiving at least one medication
cm_1 <- adsl %>%
  group_by(ARM) %>%
  summarise(n = sum(USUBJID %in% unique(cm$USUBJID)),
            total = n())
n_pct(cm_1$n, cm_1$total)


# Medication classes
cm_class <- sort(unique(cm$CMCLAS))
# Coded medication names
cm_medi <- sort(unique(cm$CMDECOD))

temp <- lapply(cm_class, function(class_i){
  adsl %>%
    group_by(ARM) %>%
    summarise(n = sum(USUBJID %in% unlist(unique(cm[cm$CMCLAS == class_i, "USUBJID"]))))
})

summarise(n = sum(USUBJID %in% cm[cm$CMCLAS %in% class_i, "USUBJID"]))

cm[cm$CMCLAS == class_i, "USUBJID"]






