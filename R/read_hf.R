#' Title
#'
#' @param from from
#' @param from.file from.file
#' @param type type
#' @param reader reader
#' @param ... ...
#'
#' @return hf
#' @export
#'
#'
read_hf <- function(from=NULL, from.file=NULL, reader=NULL, ...) {
  # Parameter checks
  assert_that(xor(is.null(from), is.null(from.file)),
              msg = "One of, and only one of, `from` or `from.file` must be populated")

  # Provided a data frame in `from``
  if (!is.null(from)) {
    assert_that(inherits(from, 'data.frame')) # from should be a data.frame/tbl
    df <- from
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

#' Title
#'
#' @param df df
#' @param required_columns required_columns
#'
#' @return hf
#' @export
#'
#'
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

# Examples code
# classes <- c('character', 'character', 'character', 'character', 'character', 'logical', 'logical', 'character', 'numeric')
# .df <- read_hf(from.file='./data/titles.csv',
#         reader=read.csv,
#         stringsAsFactors=FALSE,
#         colClasses=classes
# )
