#' Create a title line container
#'
#' @description
#' \code{hf_line} objects represent individual title or footnote lines and
#' their associated metadata. These objects are passed to an \code{rtf_doc} for
#' display in theheader or footer of an RTF document.
#'
#' A character vector of length <= 2 describes the text to display. Using a
#' single text element, the text can be aligned left, right, or center. Using
#' two text elements, the alignment can be set to “split”, which will left
#' align the first element, and right align the second. If alignment is set to
#' anything else, the text elements will be pasted together.
#'
#' Lines can either be passed in the call to \code{rtf_doc} added later with
#' \code{add_titles} or \code{add_footnotes}. Supported properties are detailed
#' in the Arguments section.
#'
#' @section Supported Formatting:
#' Several special display formats are supported to display document data. When
#' the \code{rtf_doc} is written, the package will determine if the text of an
#' \code{hf_line} object starts with a keyword. Regular expression matching and
#' replacement is used for formatting.
#' \itemize{
#' \item{PAGE_FORMAT: - Can take up to two replacements to format current
#'   page(first), and total number of pages(second). Page numbers are replaced in
#'   the string using \%s For example, for a format of Page 1 of 5, use
#'   PAGE_FORMAT: Page \%s of \%s. For a format of just 5, use
#'   PAGE_FORMAT: \%s.}
#' \item{DATE_FORMAT: - Describes the date/time the document was generated.
#'   Formats are specified using standard R date formatting tokens. Details on
#'   formatting dates can be found
#'   \href{https://www.r-bloggers.com/date-formats-in-r/}{here}.}
#' \item{FILE_PATH: - Describes the file path the R session was executed from.
#'   The location of the executing file will be populated over the token ‘%s’.
#'   Formats can be specified like “FILE_PATH: Executed from: %s” or simply
#'   “FILE_PATH: %s”. Note that the location of the executing file in R may
#'   not be intuitive. There are multiple ways to determine the containing R
#'   file based on how it was executed.
#'   \itemize{
#'     \item{When the file is executed using \code{Rscript}, this field will
#'       populated as the executed Rscript file.}
#'     \item{When the file is sourced, this field will populate with the
#'       location of the sourced file.}
#'     \item{When a file is run interactively (i.e. from the R console), this
#'       field will populate as <run interactively>.}
#'   }}
#' }
#'
#' @param ... A character list/vector. If \code{length(...)} is 2 and
#'     \code{align} is not 'split', values are pasted together.
#' @param align ext alignment in document. Options are 'center', 'left',
#'   'right', and 'split'. A 'split' alignment will left align the string in
#'    the first text item and right align the second. Defaults to center.
#' @param bold \code{TRUE} or  \code{FALSE}. Defaults to FALSE.
#' @param italic \code{TRUE} or  \code{FALSE}. Defaults to FALSE.
#' @param font A string to specify the font display. Ensure the intended RTF
#'   reader can display the selected font.
#' @param font_size Font size in half points. For example font_size = 20
#'   will display a 10 point font. Defaults to NULL, which allows default document
#'   font size to take precedence.
#' @param index Position to display header or footnote lines in the RTF
#'   document. Orders in ascending order with NULLs last.
#'
#' @return An object of class \code{hf_line} with the properties described in
#'   the Arguments section.
#'
#' @examples
#' # Adding lines during rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' titles_l <- list(
#'   hf_line(c("The Title Left", "The Titles Right"), align = "split"),
#'   hf_line("A Bold, italic Title", bold = TRUE, italic = TRUE,
#'     align = "left", font_size = 20, font = "Times New Roman")
#' )
#' rtf <- rtf_doc(ht, titles = titles_l)
#'
#' # Adding lines after rtf_doc construction
#' rtf <- add_footnotes(rtf,
#'     hf_line("PAGE_FORMAT: Page %s of %s"),
#'     hf_line("DATE_FORMAT: %H:%M %A, %B %d, %Y"),
#'     hf_line("FILE_PATH: Source: %s")
#'   )
#'
#' @export
hf_line <- function(..., align=c('center', 'left', 'right', 'split'), bold=FALSE,
                    italic=FALSE, font=NA, font_size=NULL, index=NULL) {

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
  if (!is.null(font_size)) {
    assert_that(is.numeric(font_size) && font_size %% 0.5 == 0,
                msg = "Font size must be numeric and divisible by .5")
  }
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
#'     header/footer lines. If TRUE, lines will replace the existing content.
#'
#' @return \code{rtf_doc} object with \code{hf_line} objects attached.
#' @noRd
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

#' Add \code{hf_line} title(s) and footnote(s) to a \code{rtf_doc} object
#'
#' Add \code{hf_line} titles/footnote objects to a \code{rtf_doc} object
#'
#' @param doc \code{rtf_doc} on which hf_line object(s) (i.e. titles/footnotes)
#'   will be attached
#' @param ... A vector of \code{hf_line} objects to add passed to
#'   \code{add_hf()}
#' @param replace If FALSE, lines will be appened/ordered with current
#'  header/footer lines. If TRUE, lines will replace the existing content.
#'
#' @return \code{hf_line} object(s) (i.e. titles/footnotes) to be added
#'
#' @examples
#' # Adding titles after rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#'
#' rtf <- add_titles(rtf, hf_line("The Title"))
#'
#' # Adding footnotes after rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#'
#' rtf <- add_footnotes(rtf, hf_line("The Footnote"))
#'
#' @export
#' @rdname add_titles_footnotes
add_titles <- function(doc, ..., replace=FALSE) {
  add_hf(doc, ..., to='titles', replace=replace)
}

#' @param doc \code{rtf_doc} on which hf_line object(s) (i.e. titles/footnotes)
#'   will be attached
#' @param ... A vector of \code{hf_line} objects to add passed to
#'   \code{add_hf()}
#' @param replace If FALSE, lines will be appened/ordered with current
#'   header/footer lines. If TRUE, lines will replace the existing content.
#'
#' @export
#' @rdname add_titles_footnotes
add_footnotes <- function(doc, ..., replace=FALSE) {
  add_hf(doc, ..., to='footnotes', replace=replace)
}
