#' View header and footer information
#'
#' @param doc RTF document
#' @param type type
#'
#' @import utils
#' @noRd
view_hf <- function(doc, type=NULL) {

  # Take out the lines
  lines = order_lines(doc[[type]])

  # Stop if nothing to view
  assert_that(length(lines) > 0,
              msg = sprintf('No %s to view', type))

  # Get the row length
  rows <- length(lines)

  # Create the template data frame

  .data <- data.frame(
    text1 = character(rows),
    text2 = character(rows),
    align = character(rows),
    bold = logical(rows),
    italic = logical(rows),
    font = character(rows),
    index = numeric(rows),
    stringsAsFactors = FALSE
  )

  # Loop the lines and update rows in data frame
  for (i in 1:length(lines)) {
    l <- lines[[i]]
    .data$text1[i] <- text(l)[1]
    .data$text2[i] <- text(l)[2]
    .data$align[i] <- align(l)
    .data$bold[i] <- bold(l)
    .data$italic[i] <- italic(l)
    .data$font[i] <- font(l)
    .data$index[i] <- ifelse(!is.null(index(l)), index(l), NA)
  }

  .data
}

#' View title information
#'
#' View titles attached to an \code{rtf_doc} as a data.frame.
#'
#' @param doc \code{rtf_doc} object
#'
#' @return \code{data.frame} of the title information
#' @export
view_titles <- function(doc) {
  view_hf(doc, type='titles')
}

#' View footnote information
#'
#' View footnotes attached to an \code{rtf_doc} as a data.frame.
#'
#' @param doc \code{rtf_doc} object
#'
#' @return \code{data.frame} of the footnote information
#' @export
view_footnotes <- function(doc) {
  view_hf(doc, type='footnotes')
}
