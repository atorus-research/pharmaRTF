## Header extraction methods ----
# S3 method
#' Title
#'
#' @param doc doc
#'
#' @return headers
get_column_headers <- function(doc) UseMethod('get_column_headers', object=doc$table)

get_column_headers.huxtable <- function(doc) {

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
  if (ignore_cell_padding(doc)) {
    replace_cell_padding(huxtable::to_rtf(col_headers))
  } else {
    huxtable::to_rtf(col_headers)
  }
}

get_column_headers.gt_tbl <- function(doc) {
  data <- gt:::build_data(doc$table, context='rtf')
  gt:::create_columns_component_r(data = data)
}

## Table Body extraction methods ----
# S3 method
#' Title
#'
#' @param doc doc
#'
#' @return body
get_table_body <- function(doc) UseMethod('get_table_body', object=doc$table)

# Huxtable method
get_table_body.huxtable <- function(doc) {
  # For huxtable take everything after the header riws
  start_row <- header_rows(doc$table) + 1
  body <- doc$table[start_row:nrow(doc$table), ]

  # Turn off cell padding if specified
  if (ignore_cell_padding(doc)) {
    replace_cell_padding(huxtable::to_rtf(body))
  } else {
    huxtable::to_rtf(body)
  }
}

# GT Method
get_table_body.gt_tbl <- function(doc) {
  data <- gt:::build_data(doc$table, context='rtf')
  gt:::create_body_component_r(data = data)
}
