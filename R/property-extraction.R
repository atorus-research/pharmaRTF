# Property extraction of table objects

# S3 Generic
get_fonts <- function(table, ...) UseMethod('get_fonts')

# Get all of the unique fonts from a huxtable table
get_fonts.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}



