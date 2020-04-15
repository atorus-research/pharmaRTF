## Table 14-6.02 Frequency of Normal and Abnormal (Beyond Normal Range) Laboratory Values During Treatment


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
  filter(SAFETY == "Y", LBTRMXFL == "Y")

adlbc$TRTP <- ordered(adlbc$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adlbc$LBTEST <- ordered(adlbc$LBTEST, c(
  "ALBUMIN",
  "ALKALINE PHOSPHATASE",
  "ALANINE AMINOTRANSFERASE",
  "ASPARTATE AMINOTRANSFERASE",
  "BILIRUBIN",
  "UREA NITROGEN",
  "CALCIUM",
  "CHOLESTEROL",
  "CREATINE KINASE",
  "CHLORIDE",
  "CREATININE",
  "GAMMA GLUTAMYL TRANSFERASE",
  "GLUCOSE",
  "POTASSIUM",
  "SODIUM",
  "PHOSPHATE",
  "PROTEIN",
  "URATE"
))
adlbc$LBNRIND <- ordered(adlbc$LBNRIND, c("L", "N", "H"))


adlbc2 <- adlbc %>%
  filter(LBNRIND %in% c("L", "N", "H")) %>%
  group_by(LBTEST, TRTP, LBNRIND) %>%
  complete(nesting(LBTEST, TRTP, LBNRIND)) %>%
  summarise(N = n()) %>%
  group_by(LBTEST, TRTP) %>%
  mutate(tot = sum(N)) %>%
  mutate(n_w_pct = n_pct(N, tot)) %>%
  pivot_wider(id_cols = LBTEST,names_from = c(TRTP, LBNRIND), values_from = n_w_pct)

### Heme
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", LBTRMXFL == "Y")

adlbc$TRTP <- ordered(adlbc$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adlbc$LBTEST <- ordered(adlbc$LBTEST, c(
  "BASOPHILS",
  "EOSINOPHILS",
  "HEMATOCRIT",
  "HEMOGLOBIN",
  "LYMPHOCYTES",
  "ERY. MEAN CORPUSCULAR HEMOGLOBIN",
  "ERY. MEAN CORPUSCULAR HB CONCENTRATION",
  "ERY. MEAN CORPUSCULAR VOLUME",
  "MONOCYTES",
  "PLATELET",
  "ERYTHROCYTES",
  "LEUKOCYTES"
))
adlbc$LBNRIND <- ordered(adlbc$LBNRIND, c("L", "N", "H"))


adlbh2 <- adlbh %>%
  filter(LBNRIND %in% c("L", "N", "H")) %>%
  group_by(LBTEST, TRTP, LBNRIND) %>%
  complete(nesting(LBTEST, TRTP, LBNRIND)) %>%
  summarise(N = n()) %>%
  group_by(LBTEST, TRTP) %>%
  mutate(tot = sum(N)) %>%
  mutate(n_w_pct = n_pct(N, tot)) %>%
  pivot_wider(id_cols = LBTEST, names_from = c(TRTP, LBNRIND), values_from = n)
