#
# TODO: I want to try to pass ... to dispatch instead of titles, footnotes, header.rows




supported_table_types <- c('huxtable', 'gt_tbl')

rtf_doc <- function(table, titles = list(), footnotes = list(), header.rows = 1) {
  # Return a null object of class rtf_doc if no table is passed.
  if(missing(table)) return(structure(logical(0), class = "rtf_doc"))
  as_rtf_doc(table, titles, footnotes, header.rows)
}

## Method dispatch
as_rtf_doc <- function(table, titles, footnotes, header.rows) {
  UseMethod("as_rtf_doc")
}

## For rtf_doc, return the table
as_rtf_doc.rtf_doc <- function(table, titles, footnotes, header.rows) {
  table
}

## For huxtable
as_rtf_doc.huxtable <- function(table, titles, footnotes, header.rows) {

  if (!is.na(huxtable::caption(table))) message('Huxtable contains caption - this will be stripped off ',
                                                'in RTF document generation.')

  # Huxtable table's column headers are rows of the data.frame, so store how many to grab
  attr(table, 'header.rows') <- header.rows

  new_rtf_doc(table, titles, footnotes)
}

## For GT Table
as_rtf_doc.gt_tbl <- function(table, titles, footnotes, header.rows) {


  warning('GT does not fully support RTF at this time. Results will not be as expected')

  if (!all(sapply(table[['_heading']], is.null))) {
    message('GT contains title/subtitle - this will be stripped off in RTF document generation')
  }

  if (header.rows != 1) {
    warning('The header.rows parameter has no effect on GT tables')
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

new_rtf_doc <- function(table, titles, footnotes) {

  validate_rtf_doc(table, titles, footnotes)

  # Put the object together
  doc <- list(
    table = table,
    titles = titles,
    footnotes = footnotes
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

validate_rtf_doc <- function(table, titles, footnotes) {
  # Check that titles and footnotes are lists
  assert_that(all(c(class(titles), class(footnotes)) == "list"), msg = "Titles and footnotes must be lists of hf_line objects")
  # Check that titles and footnotes are lists of hf_line objects
  assert_that(all(sapply(c(titles, footnotes), inherits, what="hf_line")),
              msg="Titles and footnotes must be lists of hf_line objects")

  # Check that header.rows is positive whole number if the table is a huxtable table
  if(inherits(table, "huxtable")){
    assert_that(
      attr(table, "header.rows") %% 1 == 0,
      attr(table, "header.rows") >= 0,
      msg = "header.rows must be a positive whole number greater than or equal to 0"
    )
  }
}
