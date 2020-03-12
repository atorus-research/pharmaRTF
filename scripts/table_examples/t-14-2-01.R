# t-14-2-01.R
#   CDISC Pilot Table 14-2.01

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')


# Import and explore the data frame ----
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt")) %>%
  filter(ITT == "Y")

get_meta(adsl)

# Create the total values upfront for quicker summary ----
adsl_ <- adsl %>%
  union(adsl %>%
          mutate(TRTPCD = 'Tot',
                 TRTP = 'Total',
                 TRTPN = 99))

# Get the header N's ----
header_n_m <- adsl_ %>%
  group_by(TRTPCD, TRTP, TRTPN) %>%
  summarize(N = n()) %>%
  mutate(
    labels = str_replace_all(str_wrap(glue('{TRTP} (N={N})'), width=10), "\n", function(x) "\\line ")
    # labels = str_wrap(glue('{TRTP} (N={N})'), width=10)
  ) %>%
  ungroup() %>%
  arrange(TRTPN) %>%
  select(-TRTP, -TRTPN)

header_n_v <- header_n_m %>% select(TRTPCD, labels) %>%
  pivot_wider(names_from = TRTPCD, values_from = labels)

header_n_v2 <- header_n_m %>% select(TRTPCD, labels) %>%
  pivot_wider(names_from = labels, values_from = TRTPCD)

# The start of some weirdness - header as named list
header <- as.list(header_n_v2)

## Exploring Age ----

# Descriptive stats
age_1 <- desc_stats(AGE)
age_p <- adsl %>% aov_p(AGE ~ TRTP) # anova

age_1 <- attach_p(age_1, age_p)

# Categorical n counts
age_2 <- sum_subgrp(AGEGRP)

agegrp_p <- adsl %>% chi_p(AGEGRP, TRTP)
age_2 <- attach_p(age_2, agegrp_p)

age <- rbind(age_1, age_2) %>%
  mutate(rowlbl1 = "Age (y)")

rm(age_1, age_2, age_p, agegrp_p)

## Exploring sex ----
sex = sum_subgrp(SEX) %>%
  mutate(rowlbl1 = "Sex",
         rowlbl2 = case_when(
           rowlbl2 == "F" ~ 'Female',
           rowlbl2 == 'M' ~ 'Male',
           rowlbl2 == 'n' ~ 'n'
         ))

sex_p <- adsl %>% chi_p(SEX, TRTP)

sex <- attach_p(sex, sex_p)

rm(sex_p)

## Exploring race ----
race = sum_subgrp(RACE) %>%
  rowwise() %>%
  mutate(
    rowlbl1 = "Race (Origin)",
    rowlbl2 =
      case_when(
        # Is there a parenthesis? If so we need to strip off that chunk
        !is.na(str_locate(rowlbl2, "\\(")[1]) ~
          # Title case it
          str_to_title(
            # Trim the white space
            str_trim(
              # Substring to first parenthesis
              str_sub(
                # str_locate to find the position of the first parenthesis (and pick it off)
                rowlbl2, 1, str_locate(rowlbl2, "\\(")[1] -1
              )
            )
          ),
        rowlbl2 == 'n' ~ 'n',
        # Catch all if none found
        TRUE ~ str_to_title(rowlbl2)
      )
  )

race_p <- adsl %>% chi_p(RACE, TRTP)

race <- attach_p(race, race_p)

rm(race_p)

## Exploring MMSE ---
mmse <- desc_stats(MMSETOT) %>%
  mutate(
    rowlbl1 = 'MMSE'
  )

mmse_p <- adsl %>% aov_p(MMSETOT ~ TRTP)

mmse <- attach_p(mmse, mmse_p)

rm(mmse_p)

## Exploring disease duration ----

# Descriptive
durdis_1 <- desc_stats(DURDIS)
durdis_1p <- adsl %>% aov_p(DURDIS ~ TRTP)
durdis_1 <- attach_p(durdis_1, durdis_1p)

# Categorical
durdis_2 <- sum_subgrp(DURDISGR)
durdis_2p <- adsl %>% chi_p(DURDISGR, TRTP)
durdis_2 <- attach_p(durdis_2, durdis_2p)

durdis <- durdis_1 %>%
  union(durdis_2) %>%
  mutate(
    rowlbl1 = 'Duration of Disease'
  )

rm(durdis_1, durdis_2, durdis_1p, durdis_2p)

## Years of education ----
educlvl <- desc_stats(EDUCLVL) %>%
  mutate(
    rowlbl1 = 'Years of education'
  )
educlvl_p <- adsl %>% aov_p(EDUCLVL ~ TRTP)
educlvl <- attach_p(educlvl, educlvl_p)

rm(educlvl_p)

## Baseline weight ----
weightbl <- desc_stats(WEIGHTBL) %>%
  mutate(
    rowlbl1 = 'Baseline weight(kg)'
  )
weightbl_p <- adsl %>% aov_p(WEIGHTBL ~ TRTP)
weightbl <- attach_p(weightbl, weightbl_p)

rm(weightbl_p)

## Baseline height ----
heightbl <- desc_stats(HEIGHTBL) %>%
  mutate(
    rowlbl1 = 'Baseline height(cm)'
  )
heightbl_p <- adsl %>% aov_p(HEIGHTBL ~ TRTP)
heightbl <- attach_p(heightbl, heightbl_p)

rm(heightbl_p)

## Baseline BMI ----

# Descriptive
bmi_1 <- desc_stats(BMIBL)
bmi_1p <- adsl %>% aov_p(BMIBL ~ TRTP)
bmi_1 <- attach_p(bmi_1, bmi_1p)

# Categorical
bmi_2 <- sum_subgrp(BMIBLGRP)
bmi_2p <- adsl %>% chi_p(BMIBLGRP, TRTP)
bmi_2 <- attach_p(bmi_2, bmi_2p)

bmi <- rbind(bmi_1, bmi_2) %>%
  mutate(
    rowlbl1 = 'Baseline BMI'
  )

rm(bmi_1, bmi_2, bmi_1p, bmi_2p)

## Stack together final tables ---
final <- rbind(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi) %>%
  group_by(rowlbl1) %>%
  mutate(ord1 = row_number()) %>%
  ungroup() %>%
  mutate(rowlbl1 = ifelse(ord1 == 1, rowlbl1, ""))

rm(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi)

## Table build and RTF Output
ht <- final %>%
  # Huxtable uses variable names so invert the header list to get the header values as variable names
  select(" "=rowlbl1, "  "=rowlbl2, !!!header, 'p-value\\line [1]'=p) %>%
  huxtable::as_hux(add_colnames=TRUE)

# ht <- as_hux(mtcars, add_colnames = TRUE)
huxtable::bottom_border(ht)[1, ] <- 1
huxtable::bold(ht)[1, ] <- TRUE
huxtable::align(ht)[1, ] <- 'center'
huxtable::width(ht) <- 1.5
huxtable::escape_contents(ht) <- FALSE
huxtable::col_width(ht) <- c(.2, .2, .12, .12, .12, .12, .12)
huxtable::bottom_padding(ht) <- 0
huxtable::top_padding(ht) <- 0

# Write into doc object and pull titles/footnotes from excel file
doc <- as_rtf_doc(ht) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-2.01')

# Set default font size for document down to 10
font_size(doc) <- 10

# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-2.01.rtf')
