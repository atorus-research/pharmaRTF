# Helper Functions

# Overwrite the base filter to be able to pass additional arguments
# Internal
Filter <- function (f, x, ...){
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}

# Identify if string is a page format
is_page_format <- function(str) {
  substr(page_fmt, 1, 12) == "PAGE_FORMAT:"
}

# Extract the format from a page format string
get_page_format <- function(str) {
  trimws(unlist(strsplit(page_fmt, ":"))[2])
}

# Extract index from an hf_line object
extract_ind <- function(x, i) {
  ind = attr(x, 'ind')
  if (is.null(ind)) return(FALSE)
  else if (ind == i) return(TRUE)
  else return(FALSE)
}
