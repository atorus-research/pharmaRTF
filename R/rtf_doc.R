
supported_table_types <- c('huxtable', 'gt_tbl')

#' Create a \code{rtf_doc} object
#'
#' @description
#' This constructs the main object that will be used for an RTF document. The
#' object is composed of a table, titles(s), and footnote(s).
#'
#' A table contained in the \code{rtf_doc} object should be a supported class.
#' The huxtable package is the most supported, however our intention is to
#' support other packages capable of writing RTF tables as well. Currently, it
#' is planned to support the \code{gt} package, but the \code{gt} package's RTF
#' methods are not functional.
#'
#' The titles and footnotes are composed of \code{hf_line} objects.
#'
#' See the <vignettes> for a more complete view of intended usage.
#'
#' @param table A table of a supported class.
#' @param titles A list of \code{hf_line} objects containing table titles and
#'   associated formatting.
#' @param footnotes A list of \code{hf_line} objects containing table footnotes
#'   and associated formatting.
#' @param header_rows An integer determining how many rows of the table are
#'   column headers. Only used for huxtable tables.
#'
#' @return A list with a table, titles, and footnotes component. Class of
#'   "rtf_doc" with the properties describled below.
#'
#' @section \code{rtf_doc} Properties:
#' Document level properties set the defaults and will be used where they are
#' not overridden by \code{hf_line} or table properties.
#' \itemize{
#' \item{font - A string representing the font to display when it is not
#'   specified by the table or \code{hf_line}. Defaults to Courier New.}
#' \item{font_size - A numeric value representing the size of the font in
#'   points. Defaults to 12.}
#' \item{margins - Inches of margins in the document as a named vector. Names
#'   are \code{top}, \code{bottom}, \code{left}, and \code{right}. Defaults to 1
#'   for all.}
#' \item{orientation - Orientation of the document. The actual height and width
#'   of the document is determined by the pagesize attribute, this is just a
#'   flag for an RTF reader. Defaults to 'landscape'.}
#' \item{header_height - Height of the header where the titles and column
#'   headers are displayed. Defaults to .5 inches.}
#' \item{footer_height - Height of the footer where the footnotes are displayed.
#'   Defaults to .5 inches.}
#' \item{pagesize - Size of the page in inches. Defaults to 8.5(height) by
#'   11(width).}
#' \item{header_rows - Huxtable table only. Number of rows that are defined as
#'   the header that will be repeated across pages. Defaults to 1}
#' \item{ignore_cell_padding - Huxtable table only. Flag to ignore cell padding padding
#'   that is added during RTF encoding. Minimizes the amount of space between
#'   rows. Defaults to FALSE.}
#' \item{column_header_buffer} - This attribute adds rows to the top or bottom of the table
#'   column headers to pad it from the titles above or the table below.
#'   Defaults to 0 and 0.
#' }
#'
#' @examples
#' # Adding lines during rtf_doc construction
#' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' # Set table properties
#' library(magrittr) #load in a pipe
#' ht %>%
#'   huxtable::set_bold(1, 1:ncol(ht), TRUE) %>%
#'   huxtable::set_escape_contents(TRUE) %>%
#'   huxtable::set_col_width(c(0.25, 0.75))
#'
#' rtf <- rtf_doc(ht, titles = list(hf_line("My Header")))
#' # Set document properties
#' rtf <- rtf %>%
#'   set_font_size(15) %>%
#'   set_ignore_cell_padding(TRUE)
#'
#' names(rtf)
#' # >[1] "table" "titles" "footnotes"
#'
#' # write_rtf(rtf, file = "filePath/rtf.rtf")
#'
#' @seealso \code{\link{hf_line}}
#'
#' @export
rtf_doc <- function(table, titles = list(), footnotes = list(), header_rows = 1) {
  # Return a null object of class rtf_doc if no table is passed.
  if(missing(table)) return(structure(logical(0), class = "rtf_doc"))
  as_rtf_doc(table, titles, footnotes, header_rows)
}

## Method dispatch
#' Create an Rich Text Format table document
#'
#' @inheritParams rtf_doc
#' @noRd
as_rtf_doc <- function(table, titles, footnotes, header_rows) {
  UseMethod("as_rtf_doc")
}

## For rtf_doc, return the table
as_rtf_doc.rtf_doc <- function(table, titles, footnotes, header_rows) {
  stop(paste('An `rtf_doc` object was provided - not a table. Supported table types are:',
        paste(supported_table_types, collapse = ", ")))
}

## For huxtable
as_rtf_doc.huxtable <- function(table, titles, footnotes, header_rows) {

  if (!is.na(huxtable::caption(table))) message('Huxtable contains caption - this will be stripped off ',
                                                'in RTF document generation.')

  # Huxtable table's column headers are rows of the data.frame, so store how many to grab
  attr(table, 'header_rows') <- header_rows

  new_rtf_doc(table, titles, footnotes)
}

## For GT Table
as_rtf_doc.gt_tbl <- function(table, titles, footnotes, header_rows) {


  warning('GT does not fully support RTF at this time. Results will not be as expected')

  if (!all(sapply(table[['_heading']], is.null))) {
    message('GT contains title/subtitle - this will be stripped off in RTF document generation')
  }

  if (header_rows != 1) {
    warning('The header_rows parameter has no effect on GT tables')
  }

  new_rtf_doc(table, titles, footnotes)

}

## Unsupported table
as_rtf_doc.default <- function(table, ...) {
  stop(paste0(
    "Unsupported table type ",
    class(table),
    "currently supported types are: ",
    supported_table_types
  ))
}

#' Create an Rich Text Format table document
#'
#' @param table A table of a supported class
#' @param titles A list of \code{hf_line} objects.
#' @param footnotes An object/list of \code{hf_line}
#'
#' @return A list with a table, titels, and footnotes component. Class of "rtf_doc"
#'
#' @noRd
new_rtf_doc <- function(table, titles, footnotes) {

  validate_rtf_doc(table, titles, footnotes)

  # Put the object together
  doc <- list(
    table = table,
    titles = order_lines(titles),
    footnotes = order_lines(footnotes)
  )

  # Create the object
  structure(doc,
            margins= c(top=1, bottom=1, left=1, right=1),
            orientation= 'landscape',
            header_height= .5,
            footer_height= .5,
            pagesize= c(height=8.5, width=11),
            font= 'Courier New',
            font_size= 12,
            ignore_cell_padding= FALSE,
            column_header_buffer= c(top=0, bottom=0),
            class= 'rtf_doc')
}

#' Validate parameters passed to rtf_doc
#'
#' @param table A table of a supported class
#' @param titles An object/list of /code{hf_line}
#' @param footnotes An object/list of /code{hf_line}
#'
#' @return nothing for now
#' @noRd
validate_rtf_doc <- function(table, titles, footnotes) {
  force(titles)
  force(footnotes)
  # Check that titles and footnotes are lists
  assert_that(all(c(class(titles), class(footnotes)) == "list"), msg = "Titles and footnotes must be lists of hf_line objects")
  # Check that titles and footnotes are lists of hf_line objects
  assert_that(all(sapply(c(titles, footnotes), inherits, what="hf_line")),
              msg="Titles and footnotes must be lists of hf_line objects")

  # Check that header_rows is positive whole number if the table is a huxtable table
  if(inherits(table, "huxtable")){
    assert_that(
      attr(table, "header_rows") %% 1 == 0,
      attr(table, "header_rows") >= 0,
      msg = "header_rows must be a positive whole number greater than or equal to 0"
    )
  }
}
