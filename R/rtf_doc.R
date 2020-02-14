#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# RTF document functions
#  - Object holding
#  - Document writing

supported_table_types <- c('huxtable')

as_rtf_doc <- function(table, titles=list(), footnotes=list()) {

  # Make sure the input tbale type is a supported class
  assert_that(
    any(sapply(supported_table_types, function(c) inherits(table, c))),
    msg=sprintf("Unsupported table type %s. Currently support types are: %s",
                class(table)[[1]],
                supported_table_types)
  )

  # Put the object together
  doc <- list(
    table = table,
    titles = titles,
    footnotes = footnotes
  )

  # Set some document properties
  attr(doc, 'margins') = c(top=1, bottom=1, left=1, right=1)
  attr(doc, 'orientation') = 'landscape'
  attr(doc, 'header_height') = .5
  attr(doc, 'footer_height') = .5
  attr(doc, 'pagesize') = c(length=8.5, width=11)

  # Set the class
  class(doc) <- 'rtf_doc'

  # Add fonts
  doc$fonts <- pharmaRTF:::font(doc)
  doc
}

library(huxtable)

ht <- as_hux(cars, add_colnames=T)
doc <- as_rtf_doc(ht)

doc <- add_hf(doc,   hf_line('line 4a', 'line 4b', align='split', index=2, font='Helvetica'),
              hf_line('line 5', index=4),
              hf_line('line 3', index=3), to='titles')
