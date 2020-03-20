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

supported_table_types <- c('huxtable', 'gt_tbl')

as_rtf_doc <- function(table, titles=list(), footnotes=list(), header.rows=1) {

  ## TODO: Come back to this and rebuild following recommended practice:
  ##       -> https://adv-r.hadley.nz/s3.html#s3-classes Section 13.3.1

  # Make sure the input tbale type is a supported class
  assert_that(
    any(sapply(supported_table_types, function(c) inherits(table, c))),
    msg=sprintf("Unsupported table type %s. Currently support types are: %s",
                class(table)[[1]],
                supported_table_types)
  )

  if (inherits(table, 'huxtable')) {
    if (!is.na(huxtable::caption(table))) message('Huxtable contains caption - this will be stripped off ',
                                        'in RTF document generation.')

    # Huxtable table's column headers are rows of the data.frame, so store how many to grab
    attr(table, 'header.rows') <- header.rows
  }
  if (inherits(table, 'gt_tbl')) {
    warning('GT does not fully support RTF at this time. Results will not be as expected')
    if (!all(sapply(table[['_heading']], is.null))) {
      message('GT contains title/subtitle - this will be stripped off in RTF document generation')
    }
  }

  # Put the object together
  doc <- list(
    table = table,
    titles = titles,
    footnotes = footnotes
  )

  # Set some document properties
  attr(doc, 'margins') <- c(top=1, bottom=1, left=1, right=1)
  attr(doc, 'orientation') <- 'landscape'
  attr(doc, 'header_height') <- .5
  attr(doc, 'footer_height') <- .5
  attr(doc, 'pagesize') <- c(height=8.5, width=11)
  attr(doc, 'font') <- 'Courier New'
  attr(doc, 'font_size') <- 12
  attr(doc, 'ignore_cell_padding') <- FALSE
  attr(doc, 'column_header_buffer') <- c(top=0, bottom=0)

  # Set the class
  class(doc) <- 'rtf_doc'

  # Add fonts from any contributing portions of the document
  # attr(doc, 'font') <- pharmaRTF:::font(doc)
  doc
}
