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

n_pct <- function(n, pct, n_width=3, pct_width=3) {
  n <- unlist(n)
  pct <- unique(pct)
  # n (%) formatted string. e.g. 50 ( 75%)
  unlist(lapply(n, function(x) {
    if(x == 0) " 0      "
    else {
      as.character(
        # Form the string using glue and format
        glue('{format(x, width=n_width)} ({format(round((x/pct) * 100), width=pct_width)}%)')
      )
    }
  }))
}

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
  mutate(tot = sum(N))

adlbc_pvals <- list()

for(i in seq(nrow(adlbc2)/9)) {
  adlbc_pvals[i] <- round(fisher.test(
    matrix(unlist(adlbc2[((i-1)*9+1):(i*9), "N"]), nrow = 3, ncol = 3, byrow = TRUE)
  )$p.value, 3)
}

adlbc3 <- adlbc2 %>%
  mutate(n_w_pct = n_pct(N, tot)) %>%
  pivot_wider(id_cols = LBTEST,names_from = c(TRTP, LBNRIND), values_from = n_w_pct) %>%
  add_column("p-val\\line[1]" = adlbc_pvals)

### Heme
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", LBTRMXFL == "Y")

adlbh$TRTP <- ordered(adlbh$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adlbh$LBTEST <- ordered(adlbh$LBTEST, c(
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
adlbh$LBNRIND <- ordered(adlbh$LBNRIND, c("L", "N", "H"))


adlbh2 <- adlbh %>%
  filter(LBNRIND %in% c("L", "N", "H")) %>%
  group_by(LBTEST, TRTP, LBNRIND) %>%
  complete(nesting(LBTEST, TRTP, LBNRIND)) %>%
  summarise(N = n()) %>%
  group_by(LBTEST, TRTP) %>%
  mutate(tot = sum(N))

adlbh_pvals <- list()

for(i in seq(nrow(adlbh2)/9)) {
  adlbh_pvals[i] <- round(fisher.test(
    matrix(unlist(adlbh2[((i-1)*9+1):(i*9), "N"]), nrow = 3, ncol = 3, byrow = TRUE)
  )$p.value, 3)
}


adlbh3 <- adlbh2 %>%
  mutate(n_w_pct = n_pct(N, tot)) %>%
  pivot_wider(id_cols = LBTEST,names_from = c(TRTP, LBNRIND), values_from = n_w_pct) %>%
  add_column("p-val\\line[1]" = adlbh_pvals)

final <- adlbc3 %>%
  ungroup() %>%
  add_row("LBTEST" = "----------", .before = 1) %>%
  add_row("LBTEST" = "CHEMISTRY", .before = 1) %>%
  add_row("LBTEST" = "", .before = 1) %>%
  add_row("LBTEST" = "") %>%
  add_row("LBTEST" = "HEMATOLOGY") %>%
  add_row("LBTEST" = "----------") %>%
  rbind(ungroup(adlbh3))


names(final) <- c(
  "",
  "Low",
  "Normal",
  "High",
  "Low",
  "Normal",
  "High",
  "Low",
  "Normal",
  "High",
  "p-val\\line[1]"
)

dm <- read_xpt(glue("{sdtm_lib}/dm.xpt"))
headers <- dm %>%
  filter(ARM != "Screen Failure") %>%
  group_by(ARM) %>%
  summarise(N = n()) %>%
  mutate(label = paste0(ARM, " (N=", N, ")"))


# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(final, header.rows = 2) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-6.02') %>%
  set_font_size(10)


# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-6.02.rtf')

