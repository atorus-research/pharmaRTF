# Property extraction of different rtf_doc and child objects ####

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####
## Fonts (getters) ----
# S3 Generic
font <- function(table, ...) UseMethod('font')

# Get all of the unique fonts from a huxtable table
font.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}

font.hf_line <- function(line) {
  attr(line, 'font')
}

font.rtf_doc <- function(doc) {
  # Get all title fonts
  titles <- sapply(doc$titles, font)
  # Get all footnote fonts
  footnotes <- sapply(doc$titles, font)
  # Get the table fonts
  table <- font(doc$table)
  # Toss them together
  combined <- c(titles, footnotes, table)
  # Remove any NA elements
  combined <- unique(combined[!is.na(combined)])
  combined
}

## Fonts (setters) ----
'font<-' <- function(x, value) UseMethod('font<-')

'font<-.hf_line' <- function(line, value) {
  attr(line, 'font') <- value
}

'font<-.rtf_doc' <- function(line, value) {
  attr(line, 'font') <- value
}

## Alignment (getters) ----
align <- function(...) UseMethod('align')

align.hf_line <- function(line) {
  attr(line, 'align')
}

## Alignment (setters) ----
'align<-' <- function(x, value) UseMethod('align<-')

'align<-.hf_line' <- function(line, value = c('left', 'right', 'center', 'split')) {
  # Check that argument is valid
  match.arg(value)

  # Check that only if alignment is split, there are two text entries
  if (length(line$text) == 1) assert_that(value != 'split',
                                        msg = 'There must be two text entries if alignment is set to split')

  attr(line, 'align') <- value
  line
}

## Bold (getters) ----
bold <- function(...) UseMethod('bold')

bold.hf_line <- function(line) {
  attr(line, 'bold')
}

## Bold (setters) ----
'bold<-' <- function(x, value) UseMethod('bold<-')

'bold<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(line, 'bold') <- value
  line
}

## Italic (getters) ----
italic <- function(...) UseMethod('italic')

italic.hf_line <- function(line) {
  attr(line, 'italic')
}


## Italic (setters) ----
'italic<-' <- function(x, value) UseMethod('italic<-')

'italic<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(line, 'italic') <- value
  line
}

## Text (getter) ----
text <- function(...) UseMethod('text')

text.hf_line <- function(line) {
  # Extract the text element
  text <- line$text

  # Add in an extra blank if only one line
  if (length(text) < 2) text <- c(text, "")

  text
}

## Text (setters) ----
'text<-' <- function(x, value) UseMethod('text<-')

'text<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  value <- unlist(value)

  # Make sure the text is character
  assert_that(is.character(value))

  #Make sure no more than two entries are provided
  assert_that(length(value) <= 2, msg = 'No more than two entries may be provided for text')

  line$text <- value
  line
}

## Bold (getters) ----
index <- function(...) UseMethod('index')

index.hf_line <- function(line) {
  attr(line, 'index')
}

## Bold (setters) ----
'index<-' <- function(x, value) UseMethod('index<-')

'index<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.numeric(value) | is.null(value))

  attr(line, 'index') <- value
  line
}

## DOCUMENT PROPERTIES ####

## Margins (getters) ----
margins <- function(...) UseMethod('margins')

margins.rtf_doc <- function(doc) {
  attr(doc, 'margins')
}

## Text (setters) ----
'margins<-' <- function(x, value) UseMethod('margins<-')

'margins<-.rtf_doc' <- function(doc, value) {

  values <- unlist(value)

  # Make sure that the parameters entered were valid
  assert_that(all(names(values) %in% c('top', 'bottom', 'left', 'right')),
              msg = 'Invalid parameter - must be top, bottom, left, or right')

  # Make sure all the entries are numeric
  lapply(values, function(x) assert_that(is.numeric(x)))

  # Save out the original margins
  margins <- margins(doc)

  # Overwrite the current margin a value was provided
  for (side in names(values)) margins[[side]] <- values[[side]]

  attr(doc, 'margins') <- margins
  doc
}


## Orientation (getters) ----
orientation <- function(...) UseMethod('orientation')

orientation.rtf_doc <- function(doc) {
  attr(doc, 'orientation')
}

## Orientation (setters) ----
'orientation<-' <- function(x, value) UseMethod('orientation<-')

'orientation<-.rtf_doc' <- function(doc, value = c('landscape', 'portrait')) {
  # Make sure the value is valid
  value <- match.arg(value)

  # Check that argument is valid
  assert_that(is.character(value))

  attr(doc, 'orientation') <- value
  doc
}

## Header height (getters)----
header_height <- function(...) UseMethod('header_height')

header_height.rtf_doc <- function(doc) {
  attr(doc, 'header_height')
}

## Header height (setters) ----
'header_height<-' <- function(x, value) UseMethod('header_height<-')

'header_height<-.rtf_doc' <- function(doc, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(doc, 'header_height') <- value
  doc
}

## Footer Height ----
footer_height <- function(...) UseMethod('footer_height')

footer_height.rtf_doc <- function(doc) {
  attr(doc, 'footer_height')
}

## Footer height (setters) ----
'footer_height<-' <- function(x, value) UseMethod('footer_height<-')

'footer_height<-.rtf_doc' <- function(doc, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(doc, 'footer_height') <- value
  doc
}

## Page Size (getters) ----
pagesize <- function(...) UseMethod('pagesize')

pagesize.rtf_doc <- function(doc) {
  attr(doc, 'pagesize')
}

## Page size(setters) ----
'pagesize<-' <- function(x, value) UseMethod('pagesize<-')

'pagesize<-.rtf_doc' <- function(doc, value) {

  values <- unlist(value)

  # Make sure that the parameters entered were valid
  assert_that(all(names(values) %in% c('length', 'width')),
              msg = 'Invalid parameters - must be length or width')

  # Make sure all the entries are numeric
  lapply(values, function(x) assert_that(is.numeric(x)))

  # Save out the original pagesize
  pagesize <- pagesize(doc)

  # Overwrite the current margin a value was provided
  for (side in names(values)) pagesize[[side]] <- values[[side]]

  attr(doc, 'pagesize') <- pagesize
  doc
}
