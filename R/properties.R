# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
#' Return or set font
#'
#' @param table
#' @param ...
#'
#' @return
#' @export
font <- function(table, ...) UseMethod('font')

#' @export
font.huxtable <- function(table) {
  unique(c(attr(table, 'font')))
}

#' @export
font.gt_tbl <- function(table) {
  character(1) # I haven't found an actual font attribute in GT? gt:::rtf_head() actually has Helvetica hard coded
}

#' @export
font.hf_line <- function(line) {
  attr(line, 'font')
}

#' @export
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
#' Title
#'
#' @param x
#' @param value
#'
#' @export
'font<-' <- function(x, value) UseMethod('font<-')

#' Title
#'
#' @param x
#' @param value
#'
#' @export
set_font <- function(x, value) UseMethod('font<-')

#' @export
'font<-.hf_line' <- function(line, value) {
  assert_that(is.character(value))
  attr(line, 'font') <- value
  line
}

#' @export
'font<-.rtf_doc' <- function(doc, value) {
  assert_that(is.character(value))
  attr(doc, 'font') <- value
  doc
}

## Font size (getters) ----
#' Title
#'
#' @param table
#' @param ...
#'
#' @return
#' @export
font_size <- function(table, ...) UseMethod('font_size')

#' @export
font_size.rtf_doc <- function(line) {
  attr(line, 'font_size')
}

#' @export
font_size.hf_line <- function(line) {
  attr(line, 'font_size')
}

## Font size (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'font_size<-' <- function(x, value) UseMethod('font_size<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_font_size <- function(x, value) UseMethod('font_size<-')

#' @export
'font_size<-.hf_line' <- function(line, value) {
  assert_that(is.numeric(value))
  attr(line, 'font_size') <- value
  line
}

#' @export
'font_size<-.rtf_doc' <- function(doc, value) {
  assert_that(is.numeric(value))
  attr(doc, 'font_size') <- value
  doc
}

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####

## Alignment (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#'
#' @export
align <- function(...) UseMethod('align')

#' @export
align.hf_line <- function(line) {
  attr(line, 'align')
}

## Alignment (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'align<-' <- function(x, value) UseMethod('align<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_align <- function(x, value) UseMethod('align<-')

#' @export
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
#' title
#'
#' @param ...
#'
#' @return
#' @export
bold <- function(...) UseMethod('bold')

#' @export
bold.hf_line <- function(line) {
  attr(line, 'bold')
}

## Bold (setters) ----
#'title
#'
#' @param x
#' @param value
#'
#' @export
'bold<-' <- function(x, value) UseMethod('bold<-')

#'title
#'
#' @param x
#' @param value
#'
#' @export
set_bold <- function(x, value) UseMethod('bold<-')

#' @export
'bold<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(line, 'bold') <- value
  line
}

## Italic (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
italic <- function(...) UseMethod('italic')

#' @export
italic.hf_line <- function(line) {
  attr(line, 'italic')
}


## Italic (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'italic<-' <- function(x, value) UseMethod('italic<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_italic <- function(x, value) UseMethod('italic<-')

#' @export
'italic<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(line, 'italic') <- value
  line
}

## Text (getter) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
text <- function(...) UseMethod('text')

#' @export
text.hf_line <- function(line) {
  # Extract the text element
  text <- line$text

  # Add in an extra blank if only one line
  if (length(text) < 2) text <- c(text, "")

  text
}

## Text (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'text<-' <- function(x, value) UseMethod('text<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_text <- function(x, value) UseMethod('text<-')

#' @export
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
#' title
#'
#' @param ...
#'
#' @return
#' @export
index <- function(...) UseMethod('index')

#' @export
index.hf_line <- function(line) {
  attr(line, 'index')
}

## Index (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'index<-' <- function(x, value) UseMethod('index<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_index <- function(x, value) UseMethod('index<-')

#' @export
'index<-.hf_line' <- function(line, value) {
  # Check that argument is valid
  assert_that(is.numeric(value) | is.null(value))

  attr(line, 'index') <- value
  line
}

## DOCUMENT PROPERTIES ####

## Margins (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
margins <- function(...) UseMethod('margins')

#' @export
margins.rtf_doc <- function(doc) {
  attr(doc, 'margins')
}

## Margins (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'margins<-' <- function(x, value) UseMethod('margins<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_margins <- function(x, value) UseMethod('margins<-')

#' @export
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
#' title
#'
#' @param ...
#'
#' @return
#' @export
orientation <- function(...) UseMethod('orientation')

#' @export
orientation.rtf_doc <- function(doc) {
  attr(doc, 'orientation')
}

## Orientation (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'orientation<-' <- function(x, value) UseMethod('orientation<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_orientation <- function(x, value) UseMethod('orientation<-')

#' @export
'orientation<-.rtf_doc' <- function(doc, value = c('landscape', 'portrait')) {
  # Make sure the value is valid
  value <- match.arg(value)

  # Check that argument is valid
  assert_that(is.character(value))

  attr(doc, 'orientation') <- value
  doc
}

## Header height (getters)----
#' title
#'
#' @param ...
#'
#' @return
#' @export
header_height <- function(...) UseMethod('header_height')

#' @export
header_height.rtf_doc <- function(doc) {
  attr(doc, 'header_height')
}

## Header height (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'header_height<-' <- function(x, value) UseMethod('header_height<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_header_height <- function(x, value) UseMethod('header_height<-')

#' @export
'header_height<-.rtf_doc' <- function(doc, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(doc, 'header_height') <- value
  doc
}

## Footer Height (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
footer_height <- function(...) UseMethod('footer_height')

#' @export
footer_height.rtf_doc <- function(doc) {
  attr(doc, 'footer_height')
}

## Footer height (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'footer_height<-' <- function(x, value) UseMethod('footer_height<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_footer_height <- function(x, value) UseMethod('footer_height<-')

#' @export
'footer_height<-.rtf_doc' <- function(doc, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(doc, 'footer_height') <- value
  doc
}

## Page Size (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
pagesize <- function(...) UseMethod('pagesize')

#' @export
pagesize.rtf_doc <- function(doc) {
  attr(doc, 'pagesize')
}

## Page size(setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'pagesize<-' <- function(x, value) UseMethod('pagesize<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_pagesize <- function(x, value) UseMethod('pagesize<-')

#' @export
'pagesize<-.rtf_doc' <- function(doc, value) {

  values <- unlist(value)

  # Make sure that the parameters entered were valid
  assert_that(all(names(values) %in% c('height', 'width')),
              msg = 'Invalid parameters - must be height or width')

  # Make sure all the entries are numeric
  lapply(values, function(x) assert_that(is.numeric(x)))

  # Save out the original pagesize
  pagesize <- pagesize(doc)

  # Overwrite the current margin a value was provided
  for (side in names(values)) pagesize[[side]] <- values[[side]]

  attr(doc, 'pagesize') <- pagesize
  doc
}

## Additional Table Properties Necessary ####

## Header rows (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
header_rows <- function(...) UseMethod('header_rows')

#' @export
header_rows.rtf_doc <- function(doc) {
  header_rows(doc$table)
}

#' @export
header_rows.huxtable <- function(table) {
  attr(table, 'header.rows')
}

#' @export
header_rows.gt_tbl <- function(table) {
  stop('GT tables do not require header rows to be set')
}

## Header rows (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'header_rows<-' <- function(x, value) UseMethod('header_rows<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_header_rows <- function(x, value) UseMethod('header_rows<-')

#' @export
'header_rows<-.rtf_doc' <- function(doc, value) {

  header_rows(doc$table) <- value
  doc
}

#' @export
'header_rows<-.huxtable' <- function(table, value) {
  # Must be a number
  assert_that(is.numeric(value) && (value %% 1 == 0), msg='Header rows must be a whole number')

  # Set the attribute
  attr(table, 'header.rows') <- value
  table
}

#' @export
'header_rows<-.gt_tbl' <- function(table, value) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
#' title
#'
#' @param ...
#'
#' @return
#' @export
ignore_cell_padding <- function(...) UseMethod('ignore_cell_padding')

#' @export
ignore_cell_padding.rtf_doc <- function(table) {
  attr(table, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
#' title
#'
#' @param x
#' @param value
#'
#' @export
'ignore_cell_padding<-' <- function(x, value) UseMethod('ignore_cell_padding<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_ignore_cell_padding <- function(x, value) UseMethod('ignore_cell_padding<-')

#' @export
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
#' title
#'
#' @param x
#' @param value
#'
#' @export
'column_header_buffer<-' <- function(x, value) UseMethod('column_header_buffer<-')

#' title
#'
#' @param x
#' @param value
#'
#' @export
set_column_header_buffer <- function(x, top, bottom) UseMethod('set_column_header_buffer')

#' @export
set_column_header_buffer.rtf_doc <- function(doc, top=0, bottom=0) {

  # Check the inputs
  valid <- all(sapply(c(top, bottom), function(x) is.numeric(x) && x%%1==0))
  assert_that(valid, msg= "Top and bottom values must be whole numbers")

  attr(doc, 'column_header_buffer') <- c(top=top, bottom=bottom)
  doc
}

#' @export
'column_header_buffer<-.rtf_doc' <- function(doc, value) {
  # Check that argument is valid
  assert_that(length(setdiff(names(value), c('top', 'bottom'))) == 0,
              msg = 'Invalid named element: only top and bottom allowed')
  # Check that values are appropriate
  valid <- all(sapply(value, function(x) is.numeric(x) && x%%1==0))
  assert_that(valid, msg= "Top and bottom values must be whole numbers")

  attr(doc, 'column_header_buffer') <- value
  doc
}
