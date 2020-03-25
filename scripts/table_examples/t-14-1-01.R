# t-14-6-01.R
#   CDISC Pilot Table 14-4.01

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in the ADLB datasets
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))

# Create the total values upfront for quicker summary ----
adsl_ <- adsl %>%
  union(adsl %>%
          mutate(TRTPCD = 'Tot',
                 TRTP = 'Total',
                 TRTPN = 99)) %>%
  mutate(
    COMPL = ifelse(DSDECOD == "PROTOCOL COMPLETED", "Y", "N")
  )

# Calculate the header Ns
header_n <- adsl_ %>%
  group_by(TRTPCD, TRTP, TRTPN) %>%
  summarize(N = n()) %>%
  mutate(
    labels = str_replace_all(str_wrap(glue('{TRTP} (N={N})'), width=10), "\n", function(x) "\\line ")
  ) %>%
  ungroup() %>%
  arrange(TRTPN) %>%
  select(-TRTP, -TRTPN)

# Column headers
header_n_v <- header_n %>% select(TRTPCD, labels) %>%
  pivot_wider(names_from = TRTPCD, values_from = labels)

# Intent to treat
itt <- sum_subgrp(ITT, include.n=FALSE) %>%
  mutate(rowlbl1 = "Intent-To-Treat (ITT)")

# Safety
safety <- sum_subgrp(SAFETY, include.n=FALSE) %>%
  mutate(rowlbl1 = "Safety")

# Efficacy
efficacy <- sum_subgrp(EFFICACY, include.n=FALSE) %>%
  mutate(rowlbl1 = "Efficacy")

# Commpleters Week 24
compl_24 <- sum_subgrp(COMPLT24, include.n=FALSE) %>%
  mutate(rowlbl1 = "Complete Week 24")

# Study completers
compl <- sum_subgrp(COMPL, include.n=FALSE) %>%
  mutate(rowlbl1 = "Complete Study")

# Pull the body together
body <- rbind(itt, safety, efficacy, compl_24, compl) %>%
           filter(rowlbl2 == "Y") %>%
           select(-rowlbl2)

# Cleanup
rm(itt, safety, efficacy, compl_24, compl)

# Attach the header
final <- bind_rows(header_n_v, body) %>%
  select(rowlbl1, Pbo, Xan_Lo, Xan_Hi, Tot)

# Make the table
ht <- as_hux(final) %>%
  huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
  huxtable::set_align(1, 1:ncol(final), 'center') %>%
  huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
  huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
  huxtable::set_width(1.1) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(c(.3, .15, .15, .15, .15))

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-1.01') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top=1)

# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-1.01.rtf')



