read_hf <- function(from=NULL, from.file=NULL, type=NULL, reader=NULL, ...) {
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
  validate_hf_dataframe(df)

}

correct_types <- function(x) {
  switch(x,
         text1=,
         text2=,
         align=,
         font='character',
         bold=,
         italic='logical'
         )
}

validate_hf_dataframe <- function(df) {

  # Flag for whether any errors were encountered
  e <- FALSE

  # Make sure the required columns all exist
  r_cols <- c("align", "bold", "font", "italic", "text1", "text2")
  cols <- sort(names(df))
  missing_cols <- setdiff(r_cols, cols)

  # Message an error for every missing column
  if (length(missing_cols) > 0) {
    e <- TRUE
    message('Error: ', paste0('\nRequired column `', missing_cols, '` is missing'))
  }

  # Check each of the required columns type for correctness
  correct <- sapply(intersect(names(df), r_cols), function(x) class(df[[x]]) == correct_types(x))

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

  # If errors were
  if (e) stop('Errors were encountered in data.frame validation.')
  }

}

# Examples code
# read_hf(from.file='./data/titles.csv',
#         reader=read.csv,
#         fileEncoding='WINDOWS-1252',
#         stringsAsFactors=FALSE,
#         colClasses=c('character', 'character', 'character', 'logical', 'logical', 'character'))
