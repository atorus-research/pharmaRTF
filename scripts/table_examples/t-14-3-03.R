# t-14-3-03.R
#   CDISC Pilot Table 14-3.03

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in the ADLB datasets ----
adas <- read_xpt(glue("{adam_lib}/adqsadas.xpt")) %>%
  filter(EFFICACY == "Y" & ITTV=='Y')

# Calculate the header Ns ----
header_n <- adas %>%
  distinct(USUBJID, TRTP, TRTPCD, TRTPN) %>%
  group_by(TRTPCD, TRTP, TRTPN) %>%
  summarize(N = n()) %>%
  mutate(
    labels = str_replace_all(str_wrap(glue('{TRTP} (N={N})'), width=10), "\n", function(x) "\\line ")
  ) %>%
  ungroup() %>%
  arrange(TRTPN) %>%
  select(-TRTP, -TRTPN)

column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRTPCD, values_from=labels) %>%
  mutate(rowlbl1 = '')

# Create the summary data portion ----
summary_data <- function(var, week, stub_header) {

  # Get the summary statistics
  df <- adas %>%
    # Filter to analsis week
    filter(AVISITCD==week) %>%
    # Summary statistics
    group_by(TRTPCD) %>%
    summarize(n = n(),
              mean = mean({{var}}),
              sd = sd({{var}}),
              median = median({{var}}),
              min = min({{var}}),
              max = max({{var}})) %>%
    # Form into display strings
    mutate(
      N = num_fmt(n, size=12, int_len=2),
      mean_sd = as.character(
        glue('{num_fmt(mean, digits=1, int_len=2, size=4)} ({num_fmt(sd, digits=2, int_len=2, size=4)})')
        ),
      med_ran = as.character(
        glue('{num_fmt(median, digits=1, int_len=2, size=4)} ({num_fmt(min, int_len=3, size=3)};{num_fmt(max, int_len=2, size=2)})')
        )
    ) %>%
    # Drop the numeric values
    select(-n, -mean, -sd, -median, -min, -max) %>%
    # Make summary stats vertical
    pivot_longer(c(N, mean_sd, med_ran), names_to = "rowlbl1") %>%
    # Split out treatment groups into separate columns
    pivot_wider(names_from=TRTPCD, values_from=value) %>%
    # Fix the row labels
    mutate(rowlbl1 = case_when(
      rowlbl1 == 'N' ~ '  n',
      rowlbl1 == 'mean_sd' ~ '  Mean (SD)',
      rowlbl1 == 'med_ran' ~ '  Median (Range)',
    ))

  # Add in the stub header
  bind_rows(tibble(rowlbl1=c(stub_header)), df)
}

# Run each group
summary_portion <- bind_rows(summary_data(VAL, 'BL', 'Baseline'),
                             summary_data(VAL, 'Wk8', 'Week 8'),
                             summary_data(CHG, 'Wk8', 'Change from Baseline')) %>%
  pad_row()

summary_portion

## P - values ####
week8 <- adas %>%
  filter(AVISITCD == 'Wk8')

## Dose Response ---
# NOTE: For statistics portions, I purposefully did not import the libraries to make it explicitly clear which
# packages were being used to match P-values.

# Need to set contrasts to work for Type III SS. See analysis results metadata for
# table 14-3.01. Reference for R here: https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/



op <- options(contrasts = c("contr.sum","contr.poly"))

# Basic linear model
model <- lm(CHG ~ TRTDOSE + SITEGRP + BASE, data=week8)

# Use `car` package Anova test with type III SS.
ancova <- car::Anova(model, type=3)

# Pull it out into a table
dose_response <- tibble(rowlbl1=c('p-value(Dose Response) [1] [2]'),
                        Xan_Hi = c(num_fmt(ancova[2, 'Pr(>F)'], int_len=4, digits=3, size=12))
  ) %>%
  pad_row()

## Pairwise Comparisons ----

# Here's a reference for the emmeans package and how to use it:
#   https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html
# Adjustments made are in line with the analysis results metadata in the analysis define
# and PROC GLM documentation.

# First need to put the factors in an appropriate level
week8['TRTPCD_F'] <- factor(week8$TRTPCD, levels=c('Xan_Hi', 'Xan_Lo', 'Pbo'))

# Linear model but use treatment group as a factor now
model <- lm(CHG ~ TRTPCD_F + SITEGRP + BASE, data=week8)
# LS Means and weight proportionately to match OM option on PROC GLM in SAS
lsm <- emmeans::lsmeans(model, ~TRTPCD_F, weights='proportional')
# Get pairwise contrast and remove P-values adjustment for multiple groups
cntrst_p <- emmeans::contrast(lsm, method="pairwise", adjust=NULL)
# 95% CI
cntrst_ci <- confint(cntrst_p)

# merge and convert into dataframe
pw_data <- as_tibble(summary(cntrst_p)) %>%
  merge(as_tibble(cntrst_ci)) %>%
  # Create the display strings
  mutate(
    p = num_fmt(p.value, int_len=4, digits=3, size=12),
    diff_se = as.character(
      glue('{num_fmt(estimate, int_len=2, digits=1, size=4)} ({num_fmt(SE, int_len=1, digits=2, size=4)})')
    ),
    ci = as.character(
      glue('({num_fmt(lower.CL, int_len=2, digits=1, size=4)};{num_fmt(upper.CL, int_len=1, digits=1, size=3)})')
    )
  ) %>%
  # Clean out the numeric variables
  select(contrast, p, diff_se, ci) %>%
  # Transpose
  pivot_longer(c('p', 'diff_se', 'ci'), names_to = 'row_label')

# Subset Xan_Lo - Pbo into table variables
xan_lo <- pw_data %>%
  filter(contrast == 'Xan_Lo - Pbo') %>%
  # Rename to the table display variable
  select(Xan_Lo=value) %>%
  pad_row()
# Add in rowlbl
xan_lo['rowlbl1'] <- c('p-value(Xan - Placebo) [1] [3]', '  Diff of LS Means (SE)', '  95% CI', '')

# Subset Xan_hi - Pbo into table variables
xan_hi <- pw_data %>%
  filter(contrast == 'Xan_Hi - Pbo') %>%
  # Rename to the table display variable
  select(Xan_Hi=value) %>%
  pad_row()
# Add in rowlbl
xan_hi['rowlbl1'] <- c('p-value(Xan - Placebo) [1] [3]', '  Diff of LS Means (SE)', '  95% CI', '')
xan_hi['ord'] <- c(1,2,3,4) # Order for sorting

# Subset Xan_Hi - Xan_Lo into table variable
xan_xan <- pw_data %>%
  filter(contrast == 'Xan_Hi - Xan_Lo') %>%
  # Rename to the table display variable
  select(Xan_Hi=value)
# Add in rowlbl
xan_xan['rowlbl1'] <- c('p-value(Xan High - Xan Low) [1] [3]', '  Diff of LS Means (SE)', '  95% CI')
xan_xan['ord'] <- c(5,6,7) # Order for sorting

# Pack it all together
pw_final <- merge(xan_lo, xan_hi, by='rowlbl1') %>%
  bind_rows(xan_xan) %>%
  arrange(ord)


final <- bind_rows(column_headers, summary_portion, dose_response, pw_final) %>%
  select(rowlbl1, Pbo, Xan_Lo, Xan_Hi)

## Create the table

# Make the table
ht <- as_hux(final) %>%
  huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
  huxtable::set_align(1, 1:ncol(final), 'center') %>%
  huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
  huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
  huxtable::set_width(1.2) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))
ht

# Write into doc object and pull titles/footnotes from excel file
## TODO: `titles_and_footnotes_from_df`` should be an exported function so remove internal reference when updated
doc <- rtf_doc(ht) %>% pharmaRTF:::titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-3.03') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top=1)

# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-3.03.rtf')



