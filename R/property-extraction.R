# Property extraction of table objects

# S3 Generic
get_fonts <- function(table, ...) UseMethod('get_fonts')

# Get all of the unique fonts from a huxtable table
get_fonts.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}

# i=4
# test
#
# Filter(test, x)
#
# # Extract index
# extract_ind <- function(x, i=NULL) {
#   ind = attr(x, 'ind')
#   if (is.null(ind)) return(FALSE)
#   else if (ind == i) return(TRUE)
#   else return(FALSE)
# }
#
# as.logical(unlist(lapply(x, extract_ind, i=1)))


