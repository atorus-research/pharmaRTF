## Make table in huxtable ----
library(huxtable)

source('./scripts/assemble_data.R')

ht <- final %>%
  # Huxtable uses variable names so invert the header list to get the header values as variable names
  select(" "=rowlbl1, "  "=rowlbl2, !!!header) %>%
  huxtable::as_hux(add_colnames=TRUE)

# ht <- as_hux(mtcars, add_colnames = TRUE)
huxtable::bottom_border(ht)[1, ] <- 1
huxtable::bold(ht)[1, ] <- TRUE
huxtable::width(ht) <- 1.5
huxtable::escape_contents(ht) <- FALSE

doc <- as_rtf_doc(ht) %>%
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

write_rtf(doc)
