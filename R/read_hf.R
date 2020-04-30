#' Read a \code{hf_line} object from a file or \code{data.frame}
#'
#' This reads in data from either an existing \code{data.frame} or a file that
#' can be read in as a
#'
#' @param from.df \code{data.frame} containing information to convert to
#'   \code{hf_line} object(s).
#' @param from.file A file that contains information to convert to
#'   \code{hf_line} object(s). Information is extracted from the file with the
#'   supplied reader.
#' @param reader A function to import the information from a file supplied.
#' @param ... Additional arguments passed to the reader.
#'
#' @return A data.frame where each row represents a header or footnote object
#'
#' @noRd
#'
#' @examples
#' #Reading from a file
#' \dontrun{
#' # read in a csv document
#' hfdf <- read_hf(from.file = "my_hf_file.csv", reader = read.csv)
#' }
#'
#'
#' @seealso \code{\link{titles_and_footnotes_from_df}}
read_hf <- function(from.df=NULL, from.file=NULL, reader=NULL, ...) {
  # Parameter checks
  assert_that(xor(is.null(from.df), is.null(from.file)),
              msg = "One of, and only one of, `from.df` or `from.file` must be populated")

  # Provided a data frame in `from`
  if (!is.null(from.df)) {
    assert_that(inherits(from.df, 'data.frame')) # from should be a data.frame/tbl
    .data <- from.df
    from.df
  }

  # Read data frame from file
  if (!is.null(from.file)){
    assert_that(file.exists(from.file)) # from.file must be a valid file
    assert_that(is.function(reader)) # custom reader must be a function

    # Read `from.file` and pass ... arguments through
    tryCatch(
      .data <- reader(from.file, ...),
      error = function(e) stop(sprintf('`reader` failed to load data.frame: %s', e))
    )
  }

  # df is loaded - make sure it's compliant
  required_columns <- c("type", "text1")
  columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "font_size", "index")

  validate_hf_dataframe(.data, required_columns, columns)
  .data <- fill_missing_data(.data, columns)
  .data[columns]

}

#' Populate missing columns and values in a \code{data.frame} of \code{hf_line} information
#'
#' Inserts missing values or missing columns of a \code{data.frame} of \code{hf_line} object
#' data. This populates all necessary information that is allowable to be left off of a
#' \code{data.frame} used in \code{titles_and_footnotes_from_df} and populates with the defaults
#' in \code{hf_line}
#'
#' @param df \code{data.frame} to populate
#' @param columns All columns for parameters of \code{hf_line}
#'
#' @noRd
fill_missing_data <- function(.data, columns) {

  # Iterate the colulmn names
  for (var in columns){
    if (var %in% names(.data)) {
      # Fill in defaults to any missing values in existing columns
      .data[is.na(.data[var]), var] <- correct_defaults(var)
    } else {
      # Insert default values for missing columns
      .data[var] <- correct_defaults(var)
    }
  }
  .data
}

#' Validate a \code{data.frame} of \code{hf_line} information
#'
#' Validates a \code{data.frame} with information that will be converted into
#' \code{hf_line} object. This function checks for column names and data types.
#'
#' @param df \code{data.frame} to validate
#' @param required_columns The required columns needed for valid \code{hf_line}
#'   objects.
#'
#' @noRd
validate_hf_dataframe <- function(df, required_columns, columns) {

  # Flag for whether any errors were encountered
  e <- FALSE

  # Make sure the required columns all exist
  cols <- sort(names(df))
  missing_cols <- setdiff(required_columns, cols)

  # Message an error for every missing column
  if (length(missing_cols) > 0) {
    e <- TRUE
    message('Error: ', paste0('\nRequired column `', missing_cols, '` is missing'))
  }

  # Check each of the required columns type for correctness
  correct <- sapply(intersect(names(df), columns), eval_type, df=df)

  # Might be obnoxious but automatically write out any incorrect column type
  if (any(!correct)) {
    e <- TRUE
    incorrect <- correct[!correct] # Get all the false names out of checked vector of attributes
    message('Error: ', paste0('\nInvalid column type: `',
                names(incorrect),
                '` must be a ',
                sapply(names(incorrect), correct_types),
                ' vector')
         )
  }

  # Check that there are no other values in title than title or footnote
  if ('type' %in% names(df)) {

    invalid_values <- setdiff(unique(df$type), c('title', 'footnote'))

    if (length(invalid_values) > 0) {
      # Show error and list each invalid value
      message('Error: Only values of "footnote" and "title" are allowed in the `type` column')
      message(paste0('       -> "', invalid_values, '" is not a valid value'))
      e <- TRUE
    }

  }

  # Check that there are no other values in title than title or footnote
  if ('align' %in% names(df)) {

    invalid_values <- setdiff(unique(df$align), c('center', 'left', 'right', 'split'))

    if (length(invalid_values) > 0) {
      # Show error and list each invalid value
      message('Error: Only values of "center", "left", "right", and "split" are allowed in the `align` column')
      message(paste0('       -> "', invalid_values, '" is not a valid value'))
      e <- TRUE
    }

  }

  # If errors were
  if (e) stop('Errors were encountered in data.frame validation.')

}

#' Read titles and footnotes from a dataframe
#'
#' Reads a data frame with header/footnote information and attaches it to an
#' \code{rtf_doc} object.The most effective way to use this function is to pass
#' information to a custom reader for your process. See Details section for more
#' information.
#'
#' @section Required Columns:
#' The following columns are required fields in a data.frame passed to
#' titles_and_footnotes_from_df:
#' \itemize{
#' \item{type(character - 'title' or 'footnote')}
#' \item{text1(character)}
#' \item{text2(character)}
#' \item{align(character - left, right, center, or split)}
#' \item{bold(logical)}
#' \item{italic(logical)}
#' \item{font(character)}
#' \item{index(numeric)}
#' }
#'
#' @details
#' Titles_and_footnotes_from_df allows you to attach titles and footnotes
#' (as hf_line objects) from a data.frame. This data.frame could be a
#' data.frame in your local environment, or read in from an external file. The
#' best way to utilize this method is to create a custom reader function. This
#' custom reader function is a function that you develop to:
#'
#' \itemize{
#' \item{Read a source file into a data.frame}
#' \item{Preprocess as necessary to keep only necessary records and variables}
#' \item{Ensure that variables are the correct data type}
#' }
#'
#' Titles_and_footnotes_from_df allows you to pass arguments into the reader
#' function, which gives you the capability to keep titles and footnotes for
#' all of your outputs in a central file and pass a filtering option, or any
#' additional parameters as necessary. For an example implementation, see our
#' vignette here. \code{vignette("tf_from_file", package = "pharmaRTF")}.
#'
#' @param doc A \code{rtf_doc} object to append header and footnote
#'   information.
#' @param from.df A \code{data.frame} object with title and footnote information.
#' @param from.file A file path to a file with title and footnote information.
#' @param reader A function to read the data from the from.file argument.
#' @param ... Parameters passed to \code{read_hf} where they are processed and
#'   constructed into \code{hf_line} objects.
#'
#' @return A \code{rtf_doc} object with header/footnote information attached.
#' @importFrom purrr transpose
#' @export
titles_and_footnotes_from_df <- function(doc, from.df=NULL, from.file=NULL, reader=NULL, ...) {

  df <- read_hf(from.df=from.df, from.file=from.file, reader=reader, ...) # Refer to read_hf in read_hf.R

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
