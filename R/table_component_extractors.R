## Header extraction methods ----
# S3 method
get_column_headers <- function(doc) UseMethod('get_column_headers', object=doc$table)

# Huxtable method
get_column_headers.huxtable <- function(doc) {
  # For huxtable it's simply the first row of the table
  huxtable::to_rtf(doc$table[1:1, ])
}

# GT Method
get_column_headers.gt_tbl <- function(doc) {
  data <- gt:::build_data(doc$table, context='rtf')
  gt:::create_columns_component_r(data = data)
}

## Table Body extraction methods ----
# S3 method
get_table_body <- function(doc) UseMethod('get_table_body', object=doc$table)

# Huxtable method
get_table_body.huxtable <- function(doc) {
  # For huxtable it's simply the first row of the table
  huxtable::to_rtf(doc$table[2:nrow(doc$table), ])
}

# GT Method
get_table_body.gt_tbl <- function(doc) {
  data <- gt:::build_data(doc$table, context='rtf')
  gt:::create_body_component_r(data = data)
}
