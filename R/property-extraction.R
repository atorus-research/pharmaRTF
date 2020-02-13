# Property extraction of table objects

# S3 Generic
get_font <- function(table, ...) UseMethod('get_font')

# Get all of the unique fonts from a huxtable table
get_font.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}

get_font.hf_line <- function(line) {
  attr(line, 'font')
}

get_font.rtf_doc <- function(doc) {
  # Get all title fonts
  titles <- sapply(doc$titles, get_font)
  # Get all footnote fonts
  footnotes <- sapply(doc$titles, get_font)
  # Get the table fonts
  table <- get_font(doc$table)
  # Toss them together
  combined <- c(titles, footnotes, table)
  # Remove any NA elements
  combined <- unique(combined[!is.na(combined)])
  combined
}


