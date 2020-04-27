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

# Buld the column headers
column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRT01PN, values_from=labels) %>%
  mutate(AETERM = '',
         p_low = "Fisher's Exact\\line p-values")

# Count of subjects with an adverse event
ae_counts <- function(.data, ..., N_counts = header_n, sort=FALSE) {

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

  # Join the subject and event counts, pivot out by treatment
  counts <- subject_counts %>%
    left_join(event_counts) %>%
    pivot_wider(id_cols=c(...), names_from=TRTAN, values_from=c(n, AEs))

  # Add in subject counts
  counts['N_0'] <- N_counts[N_counts$TRT01PN == 0, 'N']
  counts['N_54'] <- N_counts[N_counts$TRT01PN == 54, 'N']
  counts['N_81'] <- N_counts[N_counts$TRT01PN == 81, 'N']

  # Fill all NAs with 0
  counts[is.na(counts)] <- 0

  # Find no event counts
  counts['no_event_0'] <- counts$N_0 - counts$n_0
  counts['no_event_54'] <- counts$N_54 - counts$n_54
  counts['no_event_81'] <- counts$N_81 - counts$n_81

  # Calculate p-values
  counts['p_low']  <- apply(counts[, c('n_0', 'n_54', 'no_event_0', 'no_event_54')], MARGIN=1, FUN=fisher_test_ae)
  counts['p_high'] <- apply(counts[, c('n_0', 'n_81', 'no_event_0', 'no_event_81')], MARGIN=1, FUN=fisher_test_ae)

  # Formatting
  counts <- counts %>%
    rowwise() %>%
    mutate(
      npct_0  =  n_pct(n_0,  N_0,  n_width=2, pct_width=4, digits=1),
      npct_54 = n_pct(n_54, N_54, n_width=2, pct_width=4, digits=1),
      npct_81 = n_pct(n_81, N_81, n_width=2, pct_width=4, digits=1),
      cAEs_0  = paste0('[',AEs_0,  ']'),
      cAEs_54 = paste0('[',AEs_54, ']'),
      cAEs_81 = paste0('[',AEs_81, ']'),
      ord2 = AEs_81 # Use for descending event order
    )

  # Remove numeric columns not used in display
  counts <- counts %>%
    select(-starts_with('n_'), -starts_with('no_'), -starts_with('AEs'))

}

# Overall counts
overall <- ae_counts(adae) %>%
  mutate(AEBODSYS = 'ANY BODY SYSTEM', AETERM = 'ANY BODY SYSTEM', ord1=1)

# System Organ Class counts
bodsys <- ae_counts(adae, AEBODSYS) %>%
  mutate(AETERM = AEBODSYS, ord1=2) %>%
  arrange(AEBODSYS)

# Individual term counts
term <- ae_counts(adae, AEBODSYS, AETERM, sort=TRUE) %>%
  mutate(AETERM = paste0('  ', AETERM), ord1=2)

# Bring the data together
final <- bind_rows(overall, bodsys, term) %>%
  arrange(ord1, AEBODSYS, desc(ord2), AETERM)

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
