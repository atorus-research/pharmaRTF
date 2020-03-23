# t-14-6-01.R
#   CDISC Pilot Table 14-4.01

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in the ADLB datasets
adlbc <- read_xpt(glue("{adam_lib}/adlbc.xpt")) %>%
  filter(SAFETY == 'Y')
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == 'Y' & !(LBTEST %in% c('ANISOCYTOSIS', 'POIKILOCYTOSIS', 'MICROCYTES', 'MACROCYTES')))

# Template for assigning display visit values
visit_names <- data.frame(
  VISITNUM = c(1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 99),
  VISIT = c("  Bsln", "  Wk 2", "  Wk 4", "  Wk 6", "  Wk 8", "  Wk 12",
            "  Wk 16", "  Wk 20", "  Wk 24", "  Wk 26", "  End[1]"),
  stringsAsFactors = FALSE
)

test_summary <- function(x, df_=NULL) {
  # Build up the visit table and attach on the end visit (using flag)
  visits <- df_ %>%
    # Filter to the specified test
    filter(VISIT != 'UNSCHEDULED' & LBTEST == x) %>%
    union(df_ %>%
            filter(ENTMTFL == 'Y' & LBTEST == x) %>%
            mutate(VISITNUM = 99))

  # Summarize results by visit and treatment
  res <- visits %>%
    group_by(VISITNUM, TRTPCD) %>%
    summarize(n = n(),
              mean_res = mean(LBSTRESN, na.rm=TRUE),
              sd_res = sd(LBSTRESN, na.rm=TRUE))


  # Summarize change from baseline by visit and treatment
  chgbl <- visits %>%
    filter(VISITNUM != 1) %>%
    group_by(VISITNUM, TRTPCD) %>%
    summarize(mean_cbl = mean(CHSTRESN, na.rm=TRUE),
              sd_cbl = sd(CHSTRESN, na.rm=TRUE))

  # Build the display string
  df <- merge(res, chgbl, by = c('VISITNUM', 'TRTPCD'), all=TRUE) %>%
    mutate(
      N =
        ifelse(
          !is.na(n),
          num_fmt(n, size=2, int_len=2),
          ''),
      msr =
        ifelse(
          !is.na(mean_res),
          as.character(glue('{num_fmt(mean_res, size=5, digits=1, int_len=3)} ({num_fmt(sd_res, size=6, digits=2, int_len=3)})')),
          ''),
      msc =
        ifelse(
          !is.na(mean_cbl),
          as.character(glue('{num_fmt(mean_cbl, size=5, digits=1, int_len=3)} ({num_fmt(sd_cbl, size=6, digits=2, int_len=3)})')),
          '')
    ) %>%
    # Transpose the treatments out
    select(VISITNUM, TRTPCD, N, msr, msc) %>%
    pivot_wider(names_from = TRTPCD, values_from = c(N, msr, msc)) %>%
    # Merge in the visits
    merge(visit_names, by='VISITNUM') %>%
    arrange(VISITNUM) %>%
    select(VISIT, N_Pbo, msr_Pbo, msc_Pbo, N_Xan_Lo, msr_Xan_Lo, msc_Xan_Lo, N_Xan_Hi, msr_Xan_Hi, msc_Xan_Hi) %>%
    pad_row()

  # Stub header
  stub_head = data.frame(VISIT = x, stringsAsFactors = FALSE)

  final <- bind_rows(stub_head, df)
  ht <- huxtable::as_hux(final) %>%
    huxtable::merge_cells(1, 1:5)
  ht

}

add_group_head <- function(ht, group) {
  # Make a three row subset to grab names
  head_ <- ht[1:3, ]
  # Blank everything out
  head_[,] <- ''
  # First value is the group label
  head_[1, 1] <- group
  # Merge the cells
  head_ <- huxtable::merge_cells(head_, 1, 1:5)
  # Bind to the table
  rbind(head_, ht)
}

# Summarize all the chemistry data
chem <- do.call(rbind, lapply(sort(unique(adlbc$LBTEST)), test_summary, df_=adlbc)) %>%
  add_group_head('CHEMISTRY')

# Summarize all the hematology data
hema <- do.call(rbind, lapply(sort(unique(adlbh$LBTEST)), test_summary, df_=adlbh)) %>%
  add_group_head('HEMATOLOGY')

# Bind those two
ht <- rbind(chem, hema)

# Make the column headers
col_headers <- ht[5:6, ] # Stealing out a chunk of the table with no cell merging
col_headers[1, ] <- c('', 'Placebo', '', '', 'Xanomeline Low', '', '', 'Xanomeline High', '', '')
col_headers[2, ] <- c('Visit', 'N', 'Mean (SD)', 'Change\\line from Bsln\\line Mean (SD)',
                               'N', 'Mean (SD)', 'Change\\line from Bsln\\line Mean (SD)',
                               'N', 'Mean (SD)', 'Change\\line from Bsln\\line Mean (SD)')

# Now
col_headers <- col_headers %>%
  # Placebo spanner
  huxtable::merge_cells(1, 2:4) %>%
  huxtable::set_bottom_border(1, 2:4, 1) %>%
  huxtable::set_bottom_border_style(1, 2:4, 'dashed') %>%
  # Xanomeline Low spanner
  huxtable::merge_cells(1, 5:7) %>%
  huxtable::set_bottom_border(1, 5:7, 1) %>%
  huxtable::set_bottom_border_style(1, 5:7, 'dashed') %>%
  # Xanomeline High spanner
  huxtable::merge_cells(1, 8:10) %>%
  huxtable::set_bottom_border(1, 8:10, 1) %>%
  huxtable::set_bottom_border_style(1, 8:10, 'dashed') %>%
  # Bottom border
  huxtable::set_bottom_border(2, 1:10, value=1) %>%
  # bold it all
  huxtable::set_bold(value=TRUE) %>%
  huxtable::set_align(value='center') %>%
  huxtable::set_valign(value='bottom')

final <- rbind(col_headers, ht) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_col_width(1:10, value=c(.1, .025, .14, .14, .025, .14, .14, .025, .14, .14)) %>%
  huxtable::set_bottom_padding(0) %>%
  huxtable::set_top_padding(0)

# Write into doc object and pull titles/footnotes from excel file
doc <- as_rtf_doc(final, header.rows = 2) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-6.01') %>%
  set_font_size(10)

# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-6.01.rtf')



