# Helper Functions

# Overwrite the base filter to be able to pass additional arguments
# Internal
Filter <- function (f, x, ...){
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}

# Identify if string is a page format
is_page_format <- function(string) {
  substr(string, 1, 12) == "PAGE_FORMAT:"
}

# Extract the format from a page format string
get_page_format <- function(string) {
  trimws(unlist(strsplit(string, ":"))[2])
}

# Take a string of text and format it to write in a block of RTF with properties
# If determined to be a page number format, return that string
format_text_string <- function(text, properties='') {
  if (is_page_format(text)) {
    string <- add_page_num(get_page_format(text), properties=properties)
  } else {
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
