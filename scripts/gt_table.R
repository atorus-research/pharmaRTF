## Make the table in GT ----

source('./assemble_data.R')

footnote <- glue("NOTE: Duration of disease is computed as months between date of enrollment and date \\
                of onset of the first definite symptoms of Alzheimer's disease.")

t <- final %>%
  # Create the table object
  gt(rowname_col = "rowlbl2", groupname_col = "rowlbl1") %>%
  # Titles
  tab_header(
    title = "14-2.01",
    subtitle = "Summary of Demographic and Baseline Characteristics"
  ) %>%
  # Footnotes
  tab_source_note(
    source_note = footnote
  ) %>%
  # Adjust labels
  cols_label(
    Pbo = md(header_n_v$Pbo),
    Xan_Hi = md(header_n_v$Xan_Hi),
    Xan_Lo = md(header_n_v$Xan_Lo),
    Tot = md(header_n_v$Tot),
  ) %>%
  cols_move(
    columns = c('Pbo', 'Xan_Lo', 'Xan_Hi', 'Tot'),
    after = 'rowlbl2'
  ) %>%
  # Center labels
  tab_style(
    style=cell_text(align='center'),
    locations = cells_column_labels(everything()) # this formats incorrectly but noted in bug 439
  ) %>%
  # Bold the groups
  tab_style(
    style=cell_text(weight='bold'),
    locations = cells_row_groups(everything())
  )

t %>%
  gtsave('./test.html')
