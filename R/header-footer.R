library(assertthat)

## Title line container ----
#' Create a title line container
#'
#' @param ... A character list/vector
#' @param align Alignment in the document
#' @param bold Is bold?
#' @param italic Is italic?
#' @param font Font used in document, character
#' @param font_size pt of font in document, numeric
#' @param index order in document
#'
#' @return An object of class hf_line
#'
#' @export
hf_line <- function(..., align=c('center', 'left', 'right', 'split'), bold=FALSE,
                    italic=FALSE, font=NA, font_size=NaN, index=NULL) {

  line = list()

  line$text <- unlist(list(...))
  # Depending on input source NA might come through, so toss it
  line$text <- line$text[!is.na(line$text)]

  # Make sure alignment is valid
  align <- match.arg(align)

  new_hf_line(line, align, bold, italic, font, font_size, index)
}

#' Create a title line container
#'
#' @param line A character list/vector
#' @param align Alignment in the document
#' @param bold Is bold?
#' @param italic Is italic?
#' @param font Font used in document, character
#' @param font_size pt of font in document, numeric
#' @param index order in document
#'
#' @return An object of class hf_line
new_hf_line <- function(line, align, bold, italic, font, font_size, index) {

  validate_hf_line(line, align, bold, italic, font, font_size, index)

  # Assign attributes and build structure
  line <- structure(line,
                    align=align,
                    bold=bold,
                    italic=italic,
                    font=font,
                    font_size=font_size,
                    index=index,
                    class="hf_line")

  line
}

#' Validate a new title line container
#'
#' @param line A character list/vector
#' @param align Alignment in the document
#' @param bold Is bold?
#' @param italic Is italic?
#' @param font Font used in document, character
#' @param font_size pt of font in document, numeric
#' @param index order in document
#'
#' @import assertthat
#' @importFrom assertthat assert_that
validate_hf_line <- function(line, align, bold,italic, font, font_size, index) {

  # Check that no more than two entries were provided
  assert_that(length(line$text) <= 2, msg="No more than two entries may be provided per line")

  # Check that if the alignment was split, that two entries were provided
  assert_that({
    if (align == 'split') length(line$text) == 2
    else TRUE
  }, msg = "Two text entries must be provided if alignment is 'split', otherwise only one may be entered.")

  # Make sure the other arguments are logicals
  sapply(c(bold, italic), function(x) assert_that(is.logical(x)))

  # Make sure index is numeric or null
  assert_that(is.numeric(index) | is.null(index))

  # Make sure font is character
  assert_that(is.character(font) | is.na(font))

  # Make sure font size is numeric
  assert_that(is.numeric(font_size))
}

#' Title
#'
#' @param lines thelines
#'
#' @return order
#'
#'
order_lines <- function(lines) {

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
    new_lines <- append(new_lines, Filter(extract_ind, lines, i=i), after=0)
  }

  new_lines
}

#' Title
#'
#' @param doc doc
#' @param ... ...
#' @param to to
#' @param replace replace
#'
#' @return hf
#' @export
#'
#'
add_hf <- function(doc, ..., to=NULL, replace=FALSE) {

  # Get lines from doc (if specified to replace)
  if (!replace) {
    lines = doc[[to]]
    lines <- append(lines, list(...))
  } else {
    lines <- list(...)
  }

  # Make sure each provided object is an hf_line
  assert_that(all(sapply(lines, inherits, what='hf_line')),
              msg = 'Provided titles must be hf_line objects- see pharmaRTF::hf_line')

  # Sort
  lines <- order_lines(lines)

  # Add to the document object
  doc[[to]] <- lines

  doc

}

# Simplified for titles
#' Title
#'
#' @param doc doc
#' @param ... ...
#'
#' @return titles
#' @export
#'
#'
add_titles <- function(doc, ...) {
  add_hf(doc, ..., to='titles')
}

# Simplified for footnoes
#' Title
#'
#' @param doc doc
#' @param ... ...
#'
#' @return footnotes
#' @export
#'
#'
add_footnotes <- function(doc, ...) {
  add_hf(doc, ..., to='footnotes')
}

#' Read titles and footnotes from a dataframe
#'
#' @param doc RTF document
#' @param ... header and footer inormation
#'
#'
#' @import purrr
titles_and_footnotes_from_df <- function(doc, ...) {

  df <- read_hf(...) # Refer to read_hf in read_hf.R

  # Note: there's a lot of do call in here, but I'm just translating the data.frame
  # to a list, and then submitting the list as arguments to the function. See the do.call
  # documentation for more information


  # Make sure the columns are in the correct order
  df <- df[, c("type", "text1", "text2", "align", "bold", "italic", "font", "index")]

  # Subset into pieces and tranpose the rows into separate lists
  # Split off the column type column because it's just for subset
  titles_ <- transpose(df[df$type == 'title', -1])
  footnotes_ <- transpose(df[df$type == 'footnote', -1])

  # Turn all of the separate rows into hf_line objects for the titles and footnotes
  titles <- lapply(titles_, function(x) do.call(hf_line, x))
  footnotes <- lapply(footnotes_, function(x) do.call(hf_line, x))

  # Add the titles and the footnotes to the doc object
  # doc is the first argument so append that to the front of the list of titles
  doc <- do.call(add_titles, append(titles, list(doc), 0))
  doc <- do.call(add_footnotes, append(footnotes, list(doc), 0))
  doc

}
