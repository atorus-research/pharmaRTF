library(assertthat)

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

# Extract index from an hf_line object
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

# rtf_doc method
add_hf <- function(doc, ..., to=NULL) {

  # Get lines from doc
  lines = doc[[to]]

  # Add lines to be added
  lines <- append(lines, list(...))

  # Make sure each provided object is an hf_line
  assert_that(all(sapply(lines, inherits, what='hf_line')),
              msg = 'Provided titles must be hf_line objects- see pharmaRTF::hf_line')

  # Sort
  lines <- pharmaRTF:::order_hf(lines)

  # Add to the document object
  doc[[to]] <- lines

  doc

}


# Simplified for titles
add_titles <- function(doc, ...) {
  pharmaRTF:::add_hf(doc, ..., to='titles')
}

# Simplified for footnoes
add_footnoes <- function(doc, ...) {
  pharmaRTF:::add_hf(doc, ..., to='footnoes')
}

x = list(
  hf_line('line 4a', 'line 4b', align='split', index=2),
  hf_line('line 5', index=4),
  hf_line('line 3', index=3),
  hf_line('line 2', index=2),
  hf_line('line 1', index=1),
  hf_line('line 5a', 'line 5b', align='split')
)

# View titles
view_titles <- function(doc) {
  # TODO: Coerce hf_line to a data frame for easy viewing
}
