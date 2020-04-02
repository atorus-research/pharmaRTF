# Helper Functions

# Overwrite the base filter to be able to pass additional arguments
# Internal
# Internal
#' Internal filter function
#'
#' Change to \code{base::Filter()} to allow for additional arguments
#'
#' @param f a function of the appropriate arity (binary for Reduce,
#'   unary for Filter, Find and Position, k-ary for Map if this is
#'   called with k arguments). An arbitrary predicate function for Negate.
#' @param x a vector.
#' @param ... Additonal argument passed to Filter
#'
#' @return The x vector filtered by f and ...
#' @noRd
Filter <- function (f, x, ...){
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}

#' Identify if string is a page format
#'
#' @param txt A text string
#'
#' @return \code{TRUE} or \code{FALSE} if the string starts with 'PAGE_FORMAT:'
#' @noRd
is_page_format <- function(txt) {
  substr(txt, 1, 12) == "PAGE_FORMAT:"
}

#' Extract the format from a page format string
#'
#' @param txt A text string
#'
#' @return Returns everything after the first semi-colon to string out the
#'   formatting keywords.
#' @noRd
get_page_format <- function(txt) {
  # Should revisit this - but separate at the semicolon, remove the first section, and
  # patch it back together
  trimws(paste(unlist(strsplit(txt, ":"))[-1], collapse=':'))
}

#' Identify if string is a page format
#'
#' @param txt A text string
#'
#' @return \code{TRUE} or \code{FALSE} if the string starts with 'DATE_FORMAT:'
#' @noRd
#'
is_date_format <- function(txt) {
  substr(txt, 1, 12) == "DATE_FORMAT:"
}

# Extract the format from a date format string
get_date_format <- get_page_format # it's the same thing - just attach another name to it

#' Identify if string is requesting the executing file path
#'
#' @param txt A text string
#'
#' @return \code{TRUE} or \code{FALSE} if the string starts with 'FILE_PATH:'
#' @noRd
is_file_path <- function(txt) {
  substr(txt, 1, 10) == "FILE_PATH:"
}

# Get
get_filepath_format <- get_page_format # Again same idea

#' Extract the executing file path from the R Session
#'
#' @param text A text string given the format to display the filepath
#'
#' @return A text string with the filepath R session was run in or
#'   <run interactively> if ran from the console.
#' @noRd
add_filepath <- function(text){

  # This will populate if the file is sourced
  string_ <- sys.frame(1)$ofile

  # If not, go further
  if (is.null(string_)){
    # Interactively you can't be sure of location
    if (interactive()) {
      string_ <- '<run interactively>'
    } else {
      # If run in batch, use the command line arguments
      initial.options <- commandArgs(trailingOnly = FALSE)
      # File command line argument that we'll seach for
      file.arg.name <- "--file="
      # Pick that off and remove the argument syntax
      string_ <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
    }
  }
  sprintf(text, string_)
}

#' Format text string to write in a block of RTF
#'
#' Take a string of text and format it to write in a block of RTF with properties.
#' If determined to be a page number format, return that string
#'
#' @param text A text string to determine formatting
#' @param properties properties for displaying the text string
#'
#' @return A formatted text string based on keywords and properties
#' @noRd
format_text_string <- function(text, properties='') {
  if (is_page_format(text)) {
    # Page formats
    string <- add_page_num(get_page_format(text), properties=properties)
  } else if (is_date_format(text)){
    # Date formats
    string <- paste('{', properties, format(Sys.time(), get_date_format(text)), '}', sep='')
  } else if (is_file_path(text)) {
    # File paths
    string <- paste('{', properties, add_filepath(get_filepath_format(text)), '}', sep = '')
  }else {
    # Standard strings
    string <- paste('{', properties, text, '}', sep='')
  }
  string
}

#' Extract index from an hf_line object
#'
#' @param x \code{hf_line} object
#' @param i index to check for
#'
#' @return \code{TRUE} if ind == i or \code{FALSE}.
#' @noRd
extract_ind <- function(x, i) {
  ind = attr(x, 'index')
  if (is.null(ind)) return(FALSE)
  else if (ind == i) return(TRUE)
  else return(FALSE)
}

#' Return the expected type given an hf_line parameter
#'
#' @param x \code{hf_line} parameter
#'
#' @return The data type for the supplied parameter
#' @noRd
correct_types <- function(x) {
  switch(x,
         type=,
         text1=,
         text2=,
         align=,
         font='character',
         bold=,
         italic='logical',
         index='numeric'
  )
}

#' Check if the column type is valid for hf_line data.frame validation
#'
#' @param x hf_line parameter
#' @param df df
#'
#' @return type
#' @noRd
eval_type <- function(x, df) {
  # Get the command as a string
  # Using `is.` commands instead of just evaluating class because of integer vs.
  # double columns. Both are valid.
  expr_str <- paste('is.', correct_types(x), sep='')
  # Parse it into syntax
  expr <- parse(text=expr_str)
  # Evaluate
  eval(expr)(df[[x]])

}

#' Replace out RTF strings to ignore cell padding
#'
#' @param txt text of the rtf document
#'
#' @return replaced text without padding flags
#'
#' @importFrom stringr str_replace_all
#' @noRd
replace_cell_padding <- function(txt) {

  replacements <- c('\\\\clpadfl3' = '\\\\clpadfl0',
                    '\\\\clpadft3' = '\\\\clpadft0',
                    '\\\\clpadfb3' = '\\\\clpadfb0',
                    '\\\\clpadfr3' = '\\\\clpadfr0')

  stringr::str_replace_all(txt, replacements)
}

#' Helper to check if any buffer is required
#'
#' @param doc \code{rtf_doc} object
#'
#' @return \code{TRUE} if \code{rtf_doc} object has a buffer, \code{FALSE}
#'   otherwise
#' @noRd
needs_buffer <- function(doc){
  # Are either top or bottom greater than 0?
  sum(column_header_buffer(doc)) > 0
}

#' Create necessary buffer rows
#'
#' @param doc \code{rtf_doc} object
#' @param col_headers Column headers of the \code{rtf_doc}
#'
#' @return Column headers with buffers added
#' @noRd
insert_buffer <- function(doc, col_headers){

  rows <- column_header_buffer(doc)

  # Copy col headers and blank it out
  top <- col_headers[0, ]
  bottom <- col_headers[0, ]
  # Turn off borders
  huxtable::bottom_border(top) <- 0
  huxtable::bottom_border(bottom) <- 0

  # Insert the desired amount of space above
  if (rows['top'] > 0) {
    # Create the set number of blank rows
    top[1:rows['top'], ] <- ''
    # attach it to the top of the column headers
    col_headers <- rbind(top, col_headers)
  }

  if (rows['bottom'] > 0) {
    # Create hte set number of blank rows
    bottom[1:rows['bottom'], ] <- ''
    # Switch the border style and thickness down to the bottom and clean off the bottom column header row
    huxtable::bottom_border(bottom[nrow(bottom), ]) <- huxtable::bottom_border(col_headers[nrow(col_headers), ])
    huxtable::bottom_border(col_headers[nrow(col_headers), ]) <- 0
    huxtable::bottom_border_style(bottom[nrow(bottom), ]) <- huxtable::bottom_border_style(col_headers[nrow(col_headers), ])
    # Bind the space to the bottom
    col_headers <- rbind(col_headers, bottom)
  }

  col_headers
}
