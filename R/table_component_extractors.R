#' Return the RTF string for header columns
#'
#' This function relys on the package that the table originated from to create
#' a RTF string.
#'
#' @param doc \code{rtf_doc} object to return header string from.
#'
#' @return A RTF encoding string containing the header information
#' @noRd
get_column_headers <- function(doc) UseMethod('get_column_headers', object=doc$table)

#' @noRd
get_column_headers.huxtable <- function(doc) {

  if(header_rows(doc) == 0) return("")

  # Get the column header
  col_headers <- doc$table[1:header_rows(doc$table), ]

  if (needs_buffer(doc)){
    col_headers <- insert_buffer(doc, col_headers)
  }

  # Now reset the font size to the default for all the NAs because default doesn't carry forward
  cfs <- huxtable::font_size(col_headers)
  cfs[is.na(cfs)] <- font_size(doc)
  huxtable::font_size(col_headers) <- cfs

  # For huxtable simply return the header rows of the table - turn off cell padding if specified

  out <- huxtable::to_rtf(col_headers, fc_tables = huxtable::rtf_fc_tables(doc$table, extra_fonts = c("Times", font(doc))))
  if (ignore_cell_padding(doc)) {
    replace_cell_padding(out)
  } else {
    out
  }
}

# #' @noRd
# get_column_headers.gt_tbl <- function(doc) {
#   data <- gt:::build_data(doc$table, context='rtf')
#   gt:::create_columns_component_r(data = data)
# }

#' Return the RTF string for the body of a table
#'
#' This function relys on the package that the table originated from to create
#' a RTF string.
#'
#' @param doc \code{rtf_doc} object to return header string from.
#'
#' @return A RTF encoding string containing the header information
#' @noRd
get_table_body <- function(doc) UseMethod('get_table_body', object=doc$table)

#' @noRd
get_table_body.huxtable <- function(doc) {
  # For huxtable take everything after the header riws
  start_row <- header_rows(doc$table) + 1
  body <- doc$table[start_row:nrow(doc$table), ]

  # The body is written once and must be able to flow across pages. Column
  # headers are repeated by pharmaRTF, so they stay unbreakable.
  body <- make_breakable(body)

  # Turn off cell padding if specified
  out <- huxtable::to_rtf(body, fc_tables = huxtable::rtf_fc_tables(doc$table, extra_fonts = c("Times", font(doc))))
  if (ignore_cell_padding(doc)) {
    replace_cell_padding(out)
  } else {
    out
  }
}

# #' @noRd
# get_table_body.gt_tbl <- function(doc) {
#   data <- gt:::build_data(doc$table, context='rtf')
#   gt:::create_body_component_r(data = data)
# }

#' Mark a huxtable as breakable across pages
#'
#' huxtable 6.0.0 added a table level \code{breakable} property. When it is
#' FALSE (the default) \code{huxtable::to_rtf()} writes \code{\\trkeepfollow} on
#' every row but the last, which glues the whole table onto a single page. The
#' table body is written once and must be allowed to break; column headers are
#' repeated on each page by pharmaRTF and are correctly left unbreakable.
#'
#' On huxtable 5.x the property does not exist and this is a no-op.
#'
#' @param ht A \code{huxtable} object.
#'
#' @return The \code{huxtable}, marked breakable when the installed huxtable
#'   supports it.
#' @noRd
make_breakable <- function(ht) {
  if (packageVersion("huxtable") >= "6.0.0") {
    huxtable::breakable(ht) <- TRUE
  }
  ht
}
