## Make the table in GT ----

source('./scripts/assemble_data.R')
library(assertthat)
library(gt)
library(pharmaRTF)

# Create the GT table
t <- final %>%
  select(rowlbl1, rowlbl2, Pbo, Xan_Lo, Xan_Hi, Tot) %>%
  # Create the table object
  gt() %>%
  # Adjust labels
  cols_label(
    rowlbl1 = '',
    rowlbl2 = '',
    Pbo = md(header_n_v$Pbo),
    Xan_Hi = md(header_n_v$Xan_Hi),
    Xan_Lo = md(header_n_v$Xan_Lo),
    Tot = md(header_n_v$Tot),
  ) %>%
  # Center labels
  tab_style(
    style=cell_text(align='center'),
    locations = cells_column_labels(everything()) # this formats incorrectly but noted in bug 439
  ) %>%
  cols_width(
    vars(rowlbl1) ~ px(150),
    vars(rowlbl2) ~ px(150),
    vars(Pbo) ~ px(150),
    vars(Xan_Hi) ~ px(150),
    vars(Xan_Lo) ~ px(150),
    vars(Tot) ~ px(150)
  ) %>%
  tab_header('THIS IS A TITLE')

# Create the RTF document
doc <- as_rtf_doc(t) %>%
  add_titles(
    hf_line('Protocol: CDISCPILOT01', 'PAGE_FORMAT: Page %s of %s', italic=TRUE, bold=TRUE, align='split'),
    hf_line('Population: Intent-to-Treat', italic=TRUE, bold=TRUE, align='left'),
    hf_line('Table 14-2.01', italic=TRUE, bold=TRUE, align='center'),
    hf_line('Summary of Demographic and Baseline Characteristics', italic=TRUE, bold=TRUE)
  ) %>%
  add_footnotes(
    hf_line("NOTE: Duration of disease is computed as months between date of enrollment and date of onset of the first definite symptoms of Alzheimer's disease.",
            italic=TRUE, align='left')
  )

# write out
write_rtf(doc, file='./test_gt.rtf')


