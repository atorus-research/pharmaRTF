library(assertthat)

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

## Title line container ----
hf_line <- function(..., align="center", bold=FALSE, italic=FALSE, font='Courier New', index=NULL) {

  line = list()

  line$text <- list(...)

  # Check alignment
  assert_that(align %in% c('left', 'right', 'center', 'split'),
              msg="Invalid alignment entry. Must be left, right, center, or split")

  # Check that no more than two entries were provided
  assert_that(length(line$text) <= 2, msg="No more than two entries may be provided per line")

  # Check that if the alignment was split, that two entries were provided
  assert_that({
    if (align == 'split') length(line$text) == 2
    else length(line$text) == 1
  }, msg = "Two text entries must be provided if alignment is 'split', otherwise only one may be entered.")

  # Assign attributes
  attr(line, 'align') <- align
  attr(line, 'bold') <- bold
  attr(line, 'italic') <- italic
  attr(line, 'font') <- font
  attr(line, 'index') <- index

  # Assign the class
  class(line) <- 'hf_line'
  line
}

# S3 Generic
add_titles <- function(doc, ...) UseMethod('rtf_doc')

# rtf_doc method
add_hf <- function(doc, ..., to=NULL) {

  # Get lines from doc
  lines = doc[[to]]

  # Add lines to be added
  lines <- append(lines, list(...))

  # Make sure each provided object is an hf_line
  assert_that(all(sapply(titles, inherits, what='hf_line')),
              msg = 'Provided titles must be hf_line objects- see pharmaRTF::hf_line')

}

# Overwrite the base filter because I need an additional argument
Filter <- function (f, x, ...)
{
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}


# Extract index
extract_ind <- function(x, i) {
  ind = attr(x, 'ind')
  if (is.null(ind)) return(FALSE)
  else if (ind == i) return(TRUE)
  else return(FALSE)
}

# Internal function - do not export
order_hf <- function(lines) {

  # Take out the indices
  inds <- unlist(sapply(lines, FUN=attr, which='index'))

  # Make sure no indices are duplicated
  assert_that(
    !any(duplicated(inds)),
    msg = "Duplicate indices provided - ensure that provided indices are unique or NULL"
  )

  # Grab the nulls
  new_lines <- Filter(function(x) is.null(attr(x, 'index')), lines)

  # Sort the indices and reverse the order
  for (i in rev(sort(inds))) {
    # Append the items in order to the front of the list - this results in ordered lines with nulls at the back
    new_lines <- append(new_lines, pharmaRTF:::Filter(pharmaRTF:::extract_ind, lines, i=i), after=0)
  }

  new_lines
}


x = list(
  hf_line('line 5a', 'line 5b', align='split'),
  hf_line('line 3', index=3),
  hf_line('line 2', index=2),
  hf_line('line 1', index=1),
  hf_line('line 4a', 'line 4b', align='split'),
  hf_line('line 5', index=4)
)

# View titles
view_titles <- function(doc) {
  # TODO: Coerce hf_line to a data frame for easy viewing
}
