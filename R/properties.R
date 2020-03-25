# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
font <- function(x, ...) UseMethod('font')

# Get all of the unique fonts from a huxtable table
font.huxtable <- function(x) {
  unique(c(attr(x, 'font')))
}

font.gt_tbl <- function(x) {
  character(1) # I haven't found an actual font attribute in GT? gt:::rtf_head() actually has Helvetica hard coded
}

font.hf_line <- function(x) {
  attr(x, 'font')
}

font.rtf_doc <- function(x) {
  # Get all title fonts
  titles <- sapply(x$titles, font)
  # Get all footnote fonts
  footnotes <- sapply(x$footnotes, font)
  # Get the table fonts
  table <- c(font(x$table))
  # Toss them together
  combined <- c(attr(x, 'font'), titles, footnotes, table)
  # Remove any NA elements
  combined <- unlist(unique(combined[!is.na(combined)]))
  combined
}

## Fonts (setters) ----
'font<-' <- function(x, ...) UseMethod('font<-')

set_font <- function(x, ...) UseMethod('font<-')

'font<-.hf_line' <- function(x, value) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

'font<-.rtf_doc' <- function(x, value) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

## Font size (getters) ----
font_size <- function(x, ...) UseMethod('font_size')

font_size.rtf_doc <- function(x) {
  attr(x, 'font_size')
}

font_size.hf_line <- function(x) {
  attr(x, 'font_size')
}

## Font size (setters) ----
'font_size<-' <- function(x, ...) UseMethod('font_size<-')

set_font_size <- function(x, ...) UseMethod('font_size<-')

'font_size<-.hf_line' <- function(x, value) {
  assert_that(is.numeric(value))
  attr(x, 'font_size') <- value
  x
}

'font_size<-.rtf_doc' <- function(x, value) {
  assert_that(is.numeric(value))
  attr(x, 'font_size') <- value
  x
}

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####

## Alignment (getters) ----
align <- function(x, ...) UseMethod('align')

align.hf_line <- function(x) {
  attr(x, 'align')
}

## Alignment (setters) ----
'align<-' <- function(x, ...) UseMethod('align<-')

set_align <- function(x, ...) UseMethod('align<-')

'align<-.hf_line' <- function(x, value = c('left', 'right', 'center', 'split')) {
  # Check that argument is valid
  match.arg(value)

  # Check that only if alignment is split, there are two text entries
  if (length(x$text) == 1) assert_that(value != 'split',
                                        msg = 'There must be two text entries if alignment is set to split')

  attr(x, 'align') <- value
  x
}

## Bold (getters) ----
bold <- function(x, ...) UseMethod('bold')

bold.hf_line <- function(x) {
  attr(x, 'bold')
}

## Bold (setters) ----
'bold<-' <- function(x, ...) UseMethod('bold<-')

set_bold <- function(x, ...) UseMethod('bold<-')


'bold<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'bold') <- value
  x
}

## Italic (getters) ----
italic <- function(x, ...) UseMethod('italic')

italic.hf_line <- function(x) {
  attr(x, 'italic')
}


## Italic (setters) ----
'italic<-' <- function(x, ...) UseMethod('italic<-')

set_italic <- function(x, ...) UseMethod('italic<-')

'italic<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'italic') <- value
  x
}

## Text (getter) ----
text <- function(x, ...) UseMethod('text')

text.hf_line <- function(x) {
  # Extract the text element
  text <- x$text

  # Add in an extra blank if only one line
  if (length(text) < 2) text <- c(text, "")

  text
}

## Text (setters) ----
'text<-' <- function(x, ...) UseMethod('text<-')

set_text <- function(x, ...) UseMethod('text<-')

'text<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  value <- unlist(value)

  # Make sure the text is character
  assert_that(is.character(value))

  #Make sure no more than two entries are provided
  assert_that(length(value) <= 2, msg = 'No more than two entries may be provided for text')

  x$text <- value
  x
}

## Index (getters) ----
index <- function(x, ...) UseMethod('index')

index.hf_line <- function(x) {
  attr(x, 'index')
}

## Index (setters) ----
'index<-' <- function(x, ...) UseMethod('index<-')

set_index <- function(x, ...) UseMethod('index<-')

'index<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.numeric(value) | is.null(value))

  attr(x, 'index') <- value
  x
}

## DOCUMENT PROPERTIES ####

## Margins (getters) ----
margins <- function(x, ...) UseMethod('margins')

margins.rtf_doc <- function(x) {
  attr(x, 'margins')
}

## Margins (setters) ----
'margins<-' <- function(x, ...) UseMethod('margins<-')

set_margins <- function(x, ...) UseMethod('margins<-')

'margins<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Make sure that the parameters entered were valid
  assert_that(all(names(values) %in% c('top', 'bottom', 'left', 'right')),
              msg = 'Invalid parameter - must be top, bottom, left, or right')

  # Make sure duplicate parameters weren't entered
  assert_that(length(unique(names(values))) == length(names(values)),
              msg = "Duplicate parameters entered")


  # Make sure all the entries are numeric
  lapply(values, function(x) assert_that(is.numeric(x) && x >= 0, msg = "Margins must be positive numbers"))

  # Save out the original margins
  margins <- margins(x)

  # Overwrite the current margin a value was provided
  for (side in names(values)) margins[[side]] <- values[[side]]

  attr(x, 'margins') <- margins
  x
}


## Orientation (getters) ----
orientation <- function(x, ...) UseMethod('orientation')

orientation.rtf_doc <- function(x) {
  attr(x, 'orientation')
}

## Orientation (setters) ----
'orientation<-' <- function(x, ...) UseMethod('orientation<-')

set_orientation <- function(x, ...) UseMethod('orientation<-')

'orientation<-.rtf_doc' <- function(x, value = c('landscape', 'portrait')) {
  # Make sure the value is valid
  value <- match.arg(value)

  # Check that argument is valid
  assert_that(is.character(value))

  attr(x, 'orientation') <- value
  x
}

## Header height (getters)----
header_height <- function(x, ...) UseMethod('header_height')

header_height.rtf_doc <- function(x) {
  attr(x, 'header_height')
}

## Header height (setters) ----
'header_height<-' <- function(x, ...) UseMethod('header_height<-')

set_header_height <- function(x, ...) UseMethod('header_height<-')

'header_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'header_height') <- value
  x
}

## Footer Height ----
footer_height <- function(x, ...) UseMethod('footer_height')

footer_height.rtf_doc <- function(x) {
  attr(x, 'footer_height')
}

## Footer height (setters) ----
'footer_height<-' <- function(x, ...) UseMethod('footer_height<-')

set_footer_height <- function(x, ...) UseMethod('footer_height<-')

'footer_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'footer_height') <- value
  x
}

## Page Size (getters) ----
pagesize <- function(x, ...) UseMethod('pagesize')

pagesize.rtf_doc <- function(x) {
  attr(x, 'pagesize')
}

## Page size(setters) ----
'pagesize<-' <- function(x, ...) UseMethod('pagesize<-')

set_pagesize <- function(x, ...) UseMethod('pagesize<-')

'pagesize<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Make sure that the parameters entered were valid
  assert_that(all(names(values) %in% c('height', 'width')),
              msg = 'Invalid parameters - must be height or width')

  # Make sure duplicate parameters weren't entered
  assert_that(length(unique(names(values))) == length(names(values)),
              msg = "Duplicate parameters entered")

  # Make sure all the entries are numeric
  lapply(values, function(x) assert_that(is.numeric(x) && x > 0, msg="Height and Width must be positive numbers"))

  # Save out the original pagesize
  pagesize <- pagesize(x)

  # Overwrite the current margin a value was provided
  for (side in names(values)) pagesize[[side]] <- values[[side]]

  attr(x, 'pagesize') <- pagesize
  x
}

## Additional Table Properties Necessary ####

## Header rows (getters) ----
header_rows <- function(x, ...) UseMethod('header_rows')

header_rows.rtf_doc <- function(x) {
  header_rows(x$table)
}

header_rows.huxtable <- function(x) {
  attr(x, 'header.rows')
}

header_rows.gt_tbl <- function(x) {
  stop('GT tables do not require header rows to be set')
}

## Header rows (setters) ----
'header_rows<-' <- function(x, ...) UseMethod('header_rows<-')

set_header_rows <- function(x, ...) UseMethod('header_rows<-')

'header_rows<-.rtf_doc' <- function(x, value) {

  header_rows(x$table) <- value
  x
}

'header_rows<-.huxtable' <- function(x, value) {
  # Must be a number
  assert_that(is.numeric(value) && (value %% 1 == 0) && (value >= 0), msg='Header rows must be a positive whole number')

  # Set the attribute
  attr(x, 'header.rows') <- value
  x
}

'header_rows<-.gt_tbl' <- function(x, value) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding')

ignore_cell_padding.rtf_doc <- function(x) {
  attr(x, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
'ignore_cell_padding<-' <- function(x, ...) UseMethod('ignore_cell_padding<-')

set_ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding<-')


'ignore_cell_padding<-.rtf_doc' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'ignore_cell_padding') <- value
  x
}

## Column header buffer (getter)
column_header_buffer <- function(x, ...) UseMethod('column_header_buffer')

column_header_buffer.rtf_doc <- function(x) {
  attr(x, 'column_header_buffer')
}

## Ignore Cell Padding (setters) ----
'column_header_buffer<-' <- function(x, ...) UseMethod('column_header_buffer<-')

set_column_header_buffer <- function(x, ...) UseMethod('set_column_header_buffer')

set_column_header_buffer.rtf_doc <- function(x, top=0, bottom=0) {

  # Check the inputs
  valid <- all(sapply(list(top, bottom), function(x) length(x) == 1 && is.numeric(x) && x%%1==0 && x >= 0))
  assert_that(valid, msg= "Top and bottom values must be positive whole numbers")

  attr(x, 'column_header_buffer') <- c(top=top, bottom=bottom)
  x
}

'column_header_buffer<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Check that argument is valid
  assert_that(length(setdiff(names(values), c('top', 'bottom'))) == 0,
              msg = 'Invalid named element: only top and bottom allowed')

  # Make sure duplicate parameters weren't entered
  assert_that(length(unique(names(values))) == length(names(values)),
              msg = "Duplicate parameters entered")

  # Check that values are appropriate
  valid <- all(sapply(values, function(x) is.numeric(x) && x%%1==0 && x >= 0))
  assert_that(valid, msg= "Top and bottom values must be positive whole numbers")

  # Save out the original margins
  column_header_buffer <- column_header_buffer(x)

  # Overwrite the current margin a value was provided
  for (side in names(values)) column_header_buffer[[side]] <- values[[side]]

  attr(x, 'column_header_buffer') <- column_header_buffer
  x
}
