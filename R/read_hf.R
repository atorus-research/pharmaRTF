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
#' @export
#'
#' @examples
#' #Reading from a file
#' ## Not run:
#' \dontrun{
#' # read in a csv document
#' hfdf <- read_hf(from.file = "my_hf_file.csv", reader = read.csv)
#' }
#'
#' ## End(Not run)
#'
#' @seealso \code{\link{titles_and_footnotes_from_df}}
read_hf <- function(from.df=NULL, from.file=NULL, reader=NULL, ...) {
  # Parameter checks
  assert_that(xor(is.null(from.df), is.null(from.file)),
              msg = "One of, and only one of, `from.df` or `from.file` must be populated")

  # Provided a data frame in `from`
  if (!is.null(from.df)) {
    assert_that(inherits(from.df, 'data.frame')) # from should be a data.frame/tbl
    df <- from.df
    from.df
  }

  # Read data frame from file
  if (!is.null(from.file)){
    assert_that(file.exists(from.file)) # from.file must be a valid file
    assert_that(is.function(reader)) # custom reader must be a function

    # Read `from.file` and pass ... arguments through
    tryCatch(
      df <- reader(from.file, ...),
      error = function(e) stop(sprintf('`reader` failed to load data.frame: %s', e))
    )
  }

  # df is loaded - make sure it's compliant
  required_columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "index")
  validate_hf_dataframe(df, required_columns)
  df[required_columns]

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
validate_hf_dataframe <- function(df, required_columns) {

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
  correct <- sapply(intersect(names(df), required_columns), eval_type, df=df)

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


  # If errors were
  if (e) stop('Errors were encountered in data.frame validation.')

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
#' @param doc A \code{rtf_doc} object to append header and footnote information.
#' @param from.df \code{data.frame} containing information to convert to
#'   \code{hf_line} object(s).
#' @param from.file A file that contains information to convert to
#'   \code{hf_line} object(s). Information is extracted from the file with the
#'   supplied reader.
#' @param reader A function to import the information from a file supplied.
#' @param ... Parameters passed to \code{read_hf} where they are processed and
#'   constructed into \code{hf_line} objects.
#'
#' @return A \code{rtf_doc} object with header/footnote information attached.
#' @importFrom purrr transpose
#' @seealso [read_hf()] reads in each line.
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
