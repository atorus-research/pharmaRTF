#' Create a title line container
#'
#' @description
#' \code{hf_line} objects are passed to \code{rtf_doc} for display in the RTF
#' document. A list of 0, 1, or 2 strings with attributes for display.
#'
#' Add info about formatting with PAGE_FORMAT etc
#'
#' @param ... A character list/vector. If \code{length(...)} is 2 and
#'     \code{align} is not 'split', values are pasted together.
#' @param align Text alignment as left, right, center, or split. Defaults to center.
#' @param bold \code{TRUE} or  \code{FALSE}. Defaults to FALSE.
#' @param italic \code{TRUE} or  \code{FALSE}. Defaults to FALSE.
#' @param font A string to specify the font display. Ensure the intended RTF
#'   reader can display the selected font.
#' @param font_size Font size in half points. For example font_size = 20
#'   will display a 10 point font. Defaults to a 12 point font.
#' @param index Position to display header or footnote lines in the RTF
#'   document. Orderes in ascending order with NULLs last.
#'
#' @return An object of class \code{hf_line}
#'
#' @examples
#' # Adding lines during rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#' titles_l <- list(
#' hf_line("The Title")
#' )
#' rtf <- rtf_doc(ht, titles = titles_l)
#'
#' # Adding lines after rtf_doc construction
#' rtf <- add_footnotes(rtf, hf_line("The Footnote"))
#'
#' @export
hf_line <- function(..., align=c('center', 'left', 'right', 'split'), bold=FALSE,
                    italic=FALSE, font=NA, font_size=12, index=NULL) {

  line = list()

  line$text <- unlist(list(...))
  # Depending on input source NA might come through, so toss it
  line$text <- line$text[!is.na(line$text)]

  # Make sure alignment is valid
  align <- match.arg(align)

  new_hf_line(line, align, bold, italic, font, font_size, index)
}

#' @noRd
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

#' @importFrom assertthat assert_that
#'
#' @noRd
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
  assert_that(is.numeric(font_size) && font_size %% 0.5 == 0,
              msg = "Font size must be numeric and divisible by .5")
}

#' Order header/footer lines in an rtf_document
#'
#' @param lines A list/vector of \code{hf_line} objects that will be
#'   ordered.
#'
#' @return Reordered lines based on the \code{index} attribute of each line.
#'
#' @noRd
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

#' Add \code{hf_line} object(s) to a \code{rtf_doc} object
#'
#' @param doc \code{rtf_doc} object to add header/footer lines to
#' @param ... A vector of \code{hf_line} objects to add.
#' @param to Either 'titles' or 'footnotes'
#' @param replace If FALSE, lines will be appened/ordered with current
#'     header/footer lines. If TRUE, lines will replace whatever is there.
#'
#' @return \code{rtf_doc} object with \code{hf_line} objects attached.
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
              msg = paste('Provided titles must be hf_line objects, not',class(lines[[1]]),
                          '- see pharmaRTF::hf_line'))

  # Sort
  lines <- order_lines(lines)

  # Add to the document object
  doc[[to]] <- lines

  doc

}

#' Add \code{hf_line} object(s) to a \code{rtf_doc} object
#'
#' @param doc \code{rtf_doc} object to add header lines to
#' @param ... A vector of \code{hf_line} objects to add passed to
#'   \code{add_hf()}
#'
#' @return \code{rtf_doc} object with \code{hf_line} objects attached to titles.
#'
#' @examples
#' # Adding lines after rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#'
#' rtf <- add_titles(rtf, hf_line("The Footnote"))
#'
#' @export
add_titles <- function(doc, ...) {
  add_hf(doc, ..., to='titles')
}

# Simplified for footnoes
#' Add \code{hf_line} object(s) to a \code{rtf_doc} object
#'
#' @param doc \code{rtf_doc} object to add header/footer lines to
#' @param ... A vector of \code{hf_line} objects to add.
#'
#' @return \code{rtf_doc} object with \code{hf_line} objects attached.
#'
#' @examples
#' # Adding lines after rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#'
#' rtf <- add_footnotes(rtf, hf_line("The Footnote"))
#' @export
add_footnotes <- function(doc, ...) {
  add_hf(doc, ..., to='footnotes')
}

#' Read titles and footnotes from a dataframe
#'
#' Reads a data frame with header/footnote information and attaches it to an
#'   \code{rtf_doc} object.The most effective way to use this function is to pass information to a
#'   custom reader for your process. See <Vignette Link here>
#'
#' @section Required Columns:
#' \itemize{
#' \item{type}
#' \item{text1}
#' \item{text2}
#' \item{align}
#' \item{bold}
#' \item{italic}
#' \item{font}
#' \item{index}
#' }
#'
#' @param doc \code{rtf_doc} object to append header and footnote information.
#' @param ... Parameters passed to \code{read_hf} where they are processed and
#'   constructed into \code{hf_line} objects.
#'
#' @return RTF document with header/footnote information attached.
#' @importFrom purrr transpose
#' @seealso [read_hf()] reads in each line.
#' @export
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
