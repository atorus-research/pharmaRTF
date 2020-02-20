## Make the table in GT ----

source('./scripts/assemble_data.R')
library(gt)

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
  as_rtf()

t %>%
  gtsave('./test.html')

## TODO: Dig into the GT package to and make an edit of the as_rtf function. Steal the components but write just the header
##       to the titles and write the body to the body. gt::as_rtf() is located in export.R.
