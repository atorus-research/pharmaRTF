# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
font <- function(table, ...) UseMethod('font')

# Get all of the unique fonts from a huxtable table
font.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}

font.gt_tbl <- function(table) {
  character(1) # I haven't found an actual font attribute in GT? gt:::rtf_head() actually has Helvetica hard coded
}

font.hf_line <- function(line) {
  attr(line, 'font')
}

font.rtf_doc <- function(doc) {
  # Get all title fonts
  titles <- sapply(doc$titles, font)
  # Get all footnote fonts
  footnotes <- sapply(doc$footnotes, font)
  # Get the table fonts
  table <- c(font(doc$table))
  # Toss them together
  combined <- c(attr(doc, 'font'), titles, footnotes, table)
  # Remove any NA elements
  combined <- unlist(unique(combined[!is.na(combined)]))
  combined
}

## Fonts (setters) ----
'font<-' <- function(x, value) UseMethod('font<-')

set_font <- function(x, value) UseMethod('font<-')

'font<-.hf_line' <- function(line, value) {
  assert_that(is.character(value))
  attr(line, 'font') <- value
  line
}

'font<-.rtf_doc' <- function(doc, value) {
  assert_that(is.character(value))
  attr(doc, 'font') <- value
  doc
}

## Font size (getters) ----
font_size <- function(table, ...) UseMethod('font_size')

font_size.rtf_doc <- function(line) {
  attr(line, 'font_size')
}

font_size.hf_line <- function(line) {
  attr(line, 'font_size')
}

## Font size (setters) ----
'font_size<-' <- function(x, value) UseMethod('font_size<-')

set_font_size <- function(x, value) UseMethod('font_size<-')

'font_size<-.hf_line' <- function(line, value) {
  assert_that(is.numeric(value))
  attr(line, 'font_size') <- value
  line
}

'font_size<-.rtf_doc' <- function(doc, value) {
  assert_that(is.numeric(value))
  attr(doc, 'font_size') <- value
  doc
}

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####

## Alignment (getters) ----
align <- function(...) UseMethod('align')

align.hf_line <- function(line) {
  attr(line, 'align')
}

## Alignment (setters) ----
'align<-' <- function(x, value) UseMethod('align<-')

set_align <- function(x, value) UseMethod('align<-')

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

set_bold <- function(x, value) UseMethod('bold<-')


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

set_italic <- function(x, value) UseMethod('italic<-')

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

set_text <- function(x, value) UseMethod('text<-')

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

## Index (getters) ----
index <- function(...) UseMethod('index')

index.hf_line <- function(line) {
  attr(line, 'index')
}

## Index (setters) ----
'index<-' <- function(x, value) UseMethod('index<-')

set_index <- function(x, value) UseMethod('index<-')

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

## Margins (setters) ----
'margins<-' <- function(x, value) UseMethod('margins<-')

set_margins <- function(x, value) UseMethod('margins<-')

'margins<-.rtf_doc' <- function(doc, value) {

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

set_orientation <- function(x, value) UseMethod('orientation<-')

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

set_header_height <- function(x, value) UseMethod('header_height<-')

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

set_footer_height <- function(x, value) UseMethod('footer_height<-')

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

set_pagesize <- function(x, value) UseMethod('pagesize<-')

'pagesize<-.rtf_doc' <- function(doc, value) {

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
  pagesize <- pagesize(doc)

  # Overwrite the current margin a value was provided
  for (side in names(values)) pagesize[[side]] <- values[[side]]

  attr(doc, 'pagesize') <- pagesize
  doc
}

## Additional Table Properties Necessary ####

## Header rows (getters) ----
header_rows <- function(...) UseMethod('header_rows')

header_rows.rtf_doc <- function(doc) {
  header_rows(doc$table)
}

header_rows.huxtable <- function(table) {
  attr(table, 'header.rows')
}

header_rows.gt_tbl <- function(table) {
  stop('GT tables do not require header rows to be set')
}

## Header rows (setters) ----
'header_rows<-' <- function(x, value) UseMethod('header_rows<-')

set_header_rows <- function(x, value) UseMethod('header_rows<-')

'header_rows<-.rtf_doc' <- function(doc, value) {

  header_rows(doc$table) <- value
  doc
}

'header_rows<-.huxtable' <- function(table, value) {
  # Must be a number
  assert_that(is.numeric(value) && (value %% 1 == 0) && (value >= 0), msg='Header rows must be a positive whole number')

  # Set the attribute
  attr(table, 'header.rows') <- value
  table
}

'header_rows<-.gt_tbl' <- function(table, value) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
ignore_cell_padding <- function(...) UseMethod('ignore_cell_padding')

ignore_cell_padding.rtf_doc <- function(table) {
  attr(table, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
'ignore_cell_padding<-' <- function(x, value) UseMethod('ignore_cell_padding<-')

set_ignore_cell_padding <- function(x, value) UseMethod('ignore_cell_padding<-')


'ignore_cell_padding<-.rtf_doc' <- function(doc, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(doc, 'ignore_cell_padding') <- value
  doc
}

## Column header buffer (getter)
column_header_buffer <- function(...) UseMethod('column_header_buffer')

column_header_buffer.rtf_doc <- function(doc) {
  attr(doc, 'column_header_buffer')
}

## Ignore Cell Padding (setters) ----
'column_header_buffer<-' <- function(x, value) UseMethod('column_header_buffer<-')

set_column_header_buffer <- function(x, top, bottom) UseMethod('set_column_header_buffer')

set_column_header_buffer.rtf_doc <- function(doc, top=0, bottom=0) {

  # Check the inputs
  valid <- all(sapply(list(top, bottom), function(x) length(x) == 1 && is.numeric(x) && x%%1==0 && x >= 0))
  assert_that(valid, msg= "Top and bottom values must be positive whole numbers")

  attr(doc, 'column_header_buffer') <- c(top=top, bottom=bottom)
  doc
}

'column_header_buffer<-.rtf_doc' <- function(doc, value) {

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
  column_header_buffer <- column_header_buffer(doc)

  # Overwrite the current margin a value was provided
  for (side in names(values)) column_header_buffer[[side]] <- values[[side]]

  attr(doc, 'column_header_buffer') <- column_header_buffer
  doc
}
