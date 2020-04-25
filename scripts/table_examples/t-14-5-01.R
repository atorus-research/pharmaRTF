# t-14-4-01.R
#   CDISC Pilot Table 14-4.01

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in ADSL
adae <- read_xpt(glue("{adam_lib}/adae.xpt")) %>%
  filter(SAFFL == 'Y' & TRTEMFL == 'Y')

adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))

# Header N ----
header_n <- adsl %>%
  get_header_n()

column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRT01PN, values_from=labels) %>%
  mutate(rowlbl1 = '',
         p = "Fisher's Exact\\line p-values")

# Count of subjects with an adverse event
ae_counts <- function(.data, ...) {

  # Get the grouping
  grouped_data <- .data %>%
    group_by(TRTAN, TRTA, ...) %>%
    select(TRTA, TRTAN, ..., USUBJID)

  # Counts of each subject
  subject_counts <- grouped_data %>%
    distinct() %>%
    summarize(n = n())

  # Count of adverse events
  event_counts <- grouped_data %>%
    summarize(AEs = n())

  counts <- subject_counts %>%
    left_join(event_counts) %>%
    pivot_wider(id_cols=..., names_from=TRTAN, values_from=c(n, AEs))

  # Add in subject counts
  counts['N_0'] <- header_n[header_n$TRT01PN == 0, 'N']
  counts['N_54'] <- header_n[header_n$TRT01PN == 54, 'N']
  counts['N_81'] <- header_n[header_n$TRT01PN == 81, 'N']

  # Fill all NAs with 0
  counts[is.na(counts)] <- 0

  # Find no event counts
  counts['no_event_0'] <- counts$N_0 - counts$n_0
  counts['no_event_54'] <- counts$N_54 - counts$n_54
  counts['no_event_81'] <- counts$N_81 - counts$n_81

  counts
}

test <- ae_counts(adae, AEBODSYS)
test
# Order in each group by decreasing Ns in Xan_Hi

fisher_test_ae <- function(event=NULL, var=NULL, group, .data) {

  # Pull out the subset of interest
  nums <- as.double(.data[.data[var] == event, c('n_0', 'no_event_0', 'n_54', 'no_event_54')])
  # convert to a 2X2 matrix
  dim(nums) <- c(2, 2)
  return(nums)
  # Return the p-value of interest
  fisher.test(nums)$p.value
}

check <- fisher_test_ae('HEPATOBILIARY DISORDERS', 'AEBODSYS', 'asd', test)

sapply(test$AEBODSYS, fisher_test_ae, var='AEBODSYS', group=54, .data=test)

fisher_test_ae(event='CARDIAC DISORDERS', var='AEBODSYS', group=81, test)

subset <- test[test['AEBODSYS'] == 'HEPATOBILIARY DISORDERS' & test$TRTAN %in% c(0, 54), c('n', 'no_event')]


# Make the table ----

ht <- huxtable::as_hux(final) %>%
  huxtable::merge_cells(1, 3:5) %>%
  huxtable::merge_cells(1, 6:8)
huxtable::bottom_border(ht)[1, 3] <- 1
huxtable::bottom_border(ht)[1, 6] <- 1
huxtable::bottom_border(ht)[2, ] <- 1
huxtable::valign(ht)[1:2, ] <- 'bottom'
huxtable::bold(ht)[1:2, ] <- TRUE
huxtable::align(ht)[1:2, ] <- 'center'
huxtable::width(ht) <- 1.5
huxtable::escape_contents(ht) <- FALSE
huxtable::col_width(ht) <- c(.37, .1, .1, .1, .1, .1, .1, .1)
huxtable::bottom_padding(ht) <- 0
huxtable::top_padding(ht) <- 0
ht

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht, header.rows = 2) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-4.01') %>%
  set_font_size(10)

# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-4.01.rtf')
