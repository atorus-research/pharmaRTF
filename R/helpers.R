# Helper Functions

# Overwrite the base filter to be able to pass additional arguments
# Internal
Filter <- function (f, x, ...){
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}

# Identify if string is a page format
is_page_format <- function(txt) {
  substr(txt, 1, 12) == "PAGE_FORMAT:"
}

# Extract the format from a page format string
get_page_format <- function(txt) {
  # Should revisit this - but separate at the semicolon, remove the first section, and
  # patch it back together
  trimws(paste(unlist(strsplit(txt, ":"))[-1], collapse=':'))
}

# Identify if string is a page format
is_date_format <- function(txt) {
  substr(txt, 1, 12) == "DATE_FORMAT:"
}

# Extract the format from a date format string
get_date_format <- get_page_format # it's the same thing - just attach another name to it

# Identify if string is requesting the executing file path
is_file_path <- function(txt) {
  substr(txt, 1, 10) == "FILE_PATH:"
}

# Get
get_filepath_format <- get_page_format # Again same idea

# Extract the executing file path from the R Session
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

# Take a string of text and format it to write in a block of RTF with properties
# If determined to be a page number format, return that string
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

# Extract index from an hf_line object
extract_ind <- function(x, i) {
  ind = attr(x, 'index')
  if (is.null(ind)) return(FALSE)
  else if (ind == i) return(TRUE)
  else return(FALSE)
}

# Return the expected type given an hf_line parameter
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

# Check if the column type is valid for hf_line data.frame validation
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

