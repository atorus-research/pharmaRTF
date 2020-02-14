## Auto formatting page numbers ----
# TODO: Roxygen header - but this function creates the field to calculate page number
# Make sure this is an internal function
page_num <- function() {

  # TODO: Add style and font support
  page_str <- "{\\field\\flddirty{\\*\\fldinst{  PAGE   \\\\* MERGEFORMAT }}}"
  page_str
}

# TODO: Roxygen header - but this function creates the field to hold the total number of pages
# Make sure this is an internal function
page_total <- function() {

  tot_str <- "{\\field{\\*\\fldinst{ NUMPAGES}}}"
  tot_str
}

# TODO: Roxygen header - but this forms the actual page number string
add_page_num <- function(format="Page %s of %s") {

  # Make sure there's only a replacement for current and total pages
  token_ct <- unlist(gregexpr("\\%s", format))
  assert_that(length(token_ct) <= 2,
              msg = "Too many replacement strings - limited to 2 for current page and total pages.")

  # Split out the tokens of the string, apply brackets, and bring them back together
  chunks <- unlist(strsplit(format, "%s"))
  fmt_str <- paste(paste0("{", chunks, "}"), collapse="%s")

  # If the last replacement token was found at the second to last character, then it was not maintained
  # with the string split, so add it back on
  if (token_ct[length(token_ct)] == nchar(format) - 1) fmt_str <- paste(fmt_str, "%s", sep="")

  # Format in the
  page_str <- sprintf(fmt_str, page_num(type=type), page_total(type=type))
  page_str
}

## Font table ----
# Very slight modication of huxtable::font_table_string to fit with rtf_doc object
font_table_string <- function(doc){
  fonts <- get_font(doc)
  font_tbl_body <- paste0("  {\\f", seq(0, along = fonts), " ", fonts, ";}", collapse = "\n")
  paste("{\\fonttbl", font_tbl_body , "}", sep = "\n")
}

## Color Table ----
# Not investing in this as the moment so write out a default blank table
color_table_string <- function(doc){
  paste('{\\colortbl;;}')
}

doc_properties <- function(doc){


}
