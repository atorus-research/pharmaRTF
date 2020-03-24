#
# TODO: I want to try to pass ... to dispatch instead of titles, footnotes, header.rows




supported_table_types <- c('huxtable', 'gt_tbl')

#' Create an Rich Text Format table document
#'
#' @param table A table of a supported class
#' @param titles An object/list of \code{hf_line}
#' @param footnotes An object/list of \code{hf_line}
#' @param header.rows An integer determining how many rows of the table are headers.
#' @return A list with a table, titles, and footnotes component. Class of "rtf_doc"
#'
#' @export
rtf_doc <- function(table, titles = list(), footnotes = list(), header.rows = 1) {
  # Return a null object of class rtf_doc if no table is passed.
  if(missing(table)) return(structure(logical(0), class = "rtf_doc"))
  as_rtf_doc(table, titles, footnotes, header.rows)
}

## Method dispatch
#' Create an Rich Text Format table document
#'
#' @param table A table of a supported class
#' @param titles An object/list of /code{hf_line}
#' @param footnotes An object/list of /code{hf_line}
#' @param header.rows An integer determining how many rows of the table are headers.
#' @return A list with a table, titels, and footnotes component. Class of "rtf_doc"
#'
#' @family rtf_doc
as_rtf_doc <- function(table, titles, footnotes, header.rows) {
  UseMethod("as_rtf_doc")
}

as_rtf_doc.rtf_doc <- function(table, titles, footnotes, header.rows) {
  table
}

as_rtf_doc.huxtable <- function(table, titles, footnotes, header.rows) {

  if (!is.na(huxtable::caption(table))) message('Huxtable contains caption - this will be stripped off ',
                                                'in RTF document generation.')

  # Huxtable table's column headers are rows of the data.frame, so store how many to grab
  attr(table, 'header.rows') <- header.rows

  new_rtf_doc(table, titles, footnotes)
}

as_rtf_doc.gt_tbl <- function(table, titles, footnotes, header.rows) {
  warning('GT does not fully support RTF at this time. Results will not be as expected')

  if (!all(sapply(table[['_heading']], is.null))) {
    message('GT contains title/subtitle - this will be stripped off in RTF document generation')
  }

  new_rtf_doc(table, titles, footnotes)
}

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
#' @param titles An object/list of /code{hf_line}
#' @param footnotes An object/list of /code{hf_line}
#'
#' @return A list with a table, titels, and footnotes component. Class of "rtf_doc"
#'
#' @family rtf_doc
new_rtf_doc <- function(table, titles, footnotes) {

  validate_rtf_doc(table, titles, footnotes)

  # Put the object together
  doc <- list(
    table = table,
    titles = titles,
    footnotes = footnotes
  )

  # Set some document properties
  attr(doc, 'margins') <- c(top=1, bottom=1, left=1, right=1)
  attr(doc, 'orientation') <- 'landscape'
  attr(doc, 'header_height') <- .5
  attr(doc, 'footer_height') <- .5
  attr(doc, 'pagesize') <- c(height=8.5, width=11)
  attr(doc, 'font') <- 'Courier New'
  attr(doc, 'font_size') <- 12
  attr(doc, 'ignore_cell_padding') <- FALSE
  attr(doc, 'column_header_buffer') <- c(top=0, bottom=0)

  # Set the class
  class(doc) <- 'rtf_doc'

  # Add fonts from any contributing portions of the document
  # attr(doc, 'font') <- pharmaRTF:::font(doc)
  doc
}

#' Validate parameters passed to rtf_doc
#'
#' @param tables A table of a supported class
#' @param titles An object/list of /code{hf_line}
#' @param footnotes An object/list of /code{hf_line}
#' @param header.rows An integer determining how many rows of the table are headers.
#'
#' @return nothing for now
#'
#' @family rtf_doc
validate_rtf_doc <- function(tables, titles, footnotes, header.rows) {
  return()
}
