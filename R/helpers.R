# Helper Functions

# Overwrite the base filter to be able to pass additional arguments
# Internal
Filter <- function (f, x, ...)
{
  ind <- as.logical(unlist(lapply(x, f, ...)))
  x[which(ind)]
}
