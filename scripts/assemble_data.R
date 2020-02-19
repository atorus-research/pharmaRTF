# explore.R
#   Starting place for data exploration and expirimentation

source('./scripts/config.R')
source('./scripts/funcs.R')
library(glue)
library(tidyverse)
library(haven)

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

rm(adsl)

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
header

## Exploring Age ----

# Descriptive stats
age_1 <- desc_stats(AGE)

# Categorical n counts
age_2 <- sum_subgrp(AGEGRP)

age <- rbind(age_1, age_2) %>%
  mutate(rowlbl1 = "Age (y)")

rm(age_1, age_2)

## Exploring sex ----
sex = sum_subgrp(SEX) %>%
  mutate(rowlbl1 = "Sex",
         rowlbl2 = case_when(
           rowlbl2 == "F" ~ 'Female',
           rowlbl2 == 'M' ~ 'Male'
         ))

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
        # Catch all if none found
        TRUE ~ str_to_title(rowlbl2)
      )
    )

## Exploring MMSE ---
mmse <- desc_stats(MMSETOT) %>%
  mutate(
    rowlbl1 = 'MMSE'
  )

## Exploring disease duration ----

# Descriptive
durdis_1 <- desc_stats(DURDIS)

# Categorical
durdis_2 <- sum_subgrp(DURDISGR)

durdis <- durdis_1 %>%
  union(durdis_2) %>%
  mutate(
    rowlbl1 = 'Duration of Disease'
  )

rm(durdis_1, durdis_2)

## Years of education ----
educlvl <- desc_stats(EDUCLVL) %>%
  mutate(
    rowlbl1 = 'Years of education'
  )

## Baseline weight ----
weightbl <- desc_stats(WEIGHTBL) %>%
  mutate(
    rowlbl1 = 'Baseline weight(kg)'
  )

## Baseline height ----
heightbl <- desc_stats(HEIGHTBL) %>%
  mutate(
    rowlbl1 = 'Baseline height(cm)'
  )

## Baseline BMI ----

# Descriptive
bmi_1 <- desc_stats(BMIBL)

# Categorical
bmi_2 <- sum_subgrp(BMIBLGRP)

bmi <- rbind(bmi_1, bmi_2) %>%
  mutate(
    rowlbl1 = 'Baseline BMI'
  )

rm(bmi_1, bmi_2)

## Stack together final tables ---
final <- rbind(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi) %>%
  group_by(rowlbl1) %>%
  mutate(ord1 = row_number()) %>%
  ungroup() %>%
  mutate(rowlbl1 = ifelse(ord1 == 1, rowlbl1, ""))

rm(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi)
