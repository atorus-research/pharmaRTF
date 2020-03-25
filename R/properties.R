# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
#' Return or set font
#'
#' @param x table
#' @param ... ...
#'
#' @return font
#' @export
font <- function(x, ...) UseMethod('font')

#' @export
font.huxtable <- function(x, ...) {
  unique(c(attr(x, 'font')))
}

#' @export
font.gt_tbl <- function(x, ...) {
  character(1) # I haven't found an actual font attribute in GT? gt:::rtf_head() actually has Helvetica hard coded
}

#' @export
font.hf_line <- function(x, ...) {
  attr(x, 'font')
}

#' @export
font.rtf_doc <- function(x, ...) {
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
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'font<-' <- function(x, ...) UseMethod('font<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_font <- function(x, ...) UseMethod('font<-')

#' @export
'font<-.hf_line' <- function(x, value, ...) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

#' @export
'font<-.rtf_doc' <- function(x, value, ...) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

## Font size (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return font
#' @export
font_size <- function(x, ...) UseMethod('font_size')

#' @export
font_size.rtf_doc <- function(x, ...) {
  attr(x, 'font_size')
}

#' @export
font_size.hf_line <- function(x, ...) {
  attr(x, 'font_size')
}

## Font size (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'font_size<-' <- function(x, ...) UseMethod('font_size<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_font_size <- function(x, ...) UseMethod('font_size<-')

#' @export
'font_size<-.hf_line' <- function(x, value, ...) {
  assert_that(is.numeric(value))
  attr(x, 'font_size') <- value
  x
}

#' @export
'font_size<-.rtf_doc' <- function(x, value, ...) {
  assert_that(is.numeric(value))
  attr(x, 'font_size') <- value
  x
}

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####

## Alignment (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
align <- function(x, ...) UseMethod('align')

#' @export
align.hf_line <- function(x, ...) {
  attr(x, 'align')
}

## Alignment (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'align<-' <- function(x, ...) UseMethod('align<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_align <- function(x, ...) UseMethod('align<-')

#' @export
'align<-.hf_line' <- function(x, value = c('left', 'right', 'center', 'split'), ...) {
  # Check that argument is valid
  match.arg(value)

  # Check that only if alignment is split, there are two text entries
  if (length(x$text) == 1) assert_that(value != 'split',
                                        msg = 'There must be two text entries if alignment is set to split')

  attr(x, 'align') <- value
  x
}

## Bold (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
bold <- function(x, ...) UseMethod('bold')

#' @export
bold.hf_line <- function(x, ...) {
  attr(x, 'bold')
}

## Bold (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'bold<-' <- function(x, ...) UseMethod('bold<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_bold <- function(x, ...) UseMethod('bold<-')

#' @export
'bold<-.hf_line' <- function(x, value, ...) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'bold') <- value
  x
}

## Italic (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
italic <- function(x, ...) UseMethod('italic')

#' @export
italic.hf_line <- function(x, ...) {
  attr(x, 'italic')
}


## Italic (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'italic<-' <- function(x, ...) UseMethod('italic<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_italic <- function(x, ...) UseMethod('italic<-')

#' @export
'italic<-.hf_line' <- function(x, value, ...) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'italic') <- value
  x
}

## Text (getter) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
text <- function(x, ...) UseMethod('text')

#' @export
text.hf_line <- function(x, ...) {
  # Extract the text element
  text <- x$text

  # Add in an extra blank if only one line
  if (length(text) < 2) text <- c(text, "")

  text
}

## Text (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'text<-' <- function(x, ...) UseMethod('text<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_text <- function(x, ...) UseMethod('text<-')

#' @export
'text<-.hf_line' <- function(x, value, ...) {
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
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
index <- function(x, ...) UseMethod('index')

#' @export
index.hf_line <- function(x, ...) {
  attr(x, 'index')
}

## Index (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'index<-' <- function(x, ...) UseMethod('index<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_index <- function(x, ...) UseMethod('index<-')

#' @export
'index<-.hf_line' <- function(x, value, ...) {
  # Check that argument is valid
  assert_that(is.numeric(value) | is.null(value))

  attr(x, 'index') <- value
  x
}

## DOCUMENT PROPERTIES ####

## Margins (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#' @export
margins <- function(x, ...) UseMethod('margins')

#' @export
margins.rtf_doc <- function(x, ...) {
  attr(x, 'margins')
}

## Margins (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'margins<-' <- function(x, ...) UseMethod('margins<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_margins <- function(x, ...) UseMethod('margins<-')

#' @export
'margins<-.rtf_doc' <- function(x, value, ...) {

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
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
orientation <- function(x, ...) UseMethod('orientation')

#' @export
orientation.rtf_doc <- function(x, ...) {
  attr(x, 'orientation')
}

## Orientation (setters) ----
#' Title
#'
#' @param x x
#' @param value ...
#'
#' @export
'orientation<-' <- function(x, ...) UseMethod('orientation<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_orientation <- function(x, ...) UseMethod('orientation<-')

#' @export
'orientation<-.rtf_doc' <- function(x, value = c('landscape', 'portrait'), ...) {
  # Make sure the value is valid
  value <- match.arg(value)

  # Check that argument is valid
  assert_that(is.character(value))

  attr(x, 'orientation') <- value
  x
}

## Header height (getters)----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
header_height <- function(x, ...) UseMethod('header_height')

#' @export
header_height.rtf_doc <- function(x, ...) {
  attr(x, 'header_height')
}

## Header height (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'header_height<-' <- function(x, ...) UseMethod('header_height<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_header_height <- function(x, ...) UseMethod('header_height<-')

#' @export
'header_height<-.rtf_doc' <- function(x, value, ...) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'header_height') <- value
  x
}

## Footer Height ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
footer_height <- function(x, ...) UseMethod('footer_height')

#' @export
footer_height.rtf_doc <- function(x, ...) {
  attr(x, 'footer_height')
}

## Footer height (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'footer_height<-' <- function(x, ...) UseMethod('footer_height<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_footer_height <- function(x, ...) UseMethod('footer_height<-')

#' @export
'footer_height<-.rtf_doc' <- function(x, value, ...) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'footer_height') <- value
  x
}

## Page Size (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
pagesize <- function(x, ...) UseMethod('pagesize')

#' @export
pagesize.rtf_doc <- function(x, ...) {
  attr(x, 'pagesize')
}

## Page size(setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'pagesize<-' <- function(x, ...) UseMethod('pagesize<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_pagesize <- function(x, ...) UseMethod('pagesize<-')

#' @export
'pagesize<-.rtf_doc' <- function(x, value, ...) {

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
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
header_rows <- function(x, ...) UseMethod('header_rows')

#' @export
header_rows.rtf_doc <- function(x, ...) {
  header_rows(x$table)
}

#' @export
header_rows.huxtable <- function(x, ...) {
  attr(x, 'header.rows')
}

#' @export
header_rows.gt_tbl <- function(x, ...) {
  stop('GT tables do not require header rows to be set')
}

## Header rows (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'header_rows<-' <- function(x, ...) UseMethod('header_rows<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_header_rows <- function(x, ...) UseMethod('header_rows<-')

#' @export
'header_rows<-.rtf_doc' <- function(x, value, ...) {

  header_rows(x$table) <- value
  x
}

#' @export
'header_rows<-.huxtable' <- function(x, value, ...) {
  # Must be a number
  assert_that(is.numeric(value) && (value %% 1 == 0) && (value >= 0), msg='Header rows must be a positive whole number')

  # Set the attribute
  attr(x, 'header.rows') <- value
  x
}

#' @export
'header_rows<-.gt_tbl' <- function(x, value, ...) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding')

#' @export
ignore_cell_padding.rtf_doc <- function(x, ...) {
  attr(x, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'ignore_cell_padding<-' <- function(x, ...) UseMethod('ignore_cell_padding<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding<-')

#' @export
'ignore_cell_padding<-.rtf_doc' <- function(x, value, ...) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'ignore_cell_padding') <- value
  x
}

## Column header buffer (getter)
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @return return
#'
#' @export
column_header_buffer <- function(x, ...) UseMethod('column_header_buffer')

#' @export
column_header_buffer.rtf_doc <- function(x, ...) {
  attr(x, 'column_header_buffer')
}

## Ignore Cell Padding (setters) ----
#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
'column_header_buffer<-' <- function(x, ...) UseMethod('column_header_buffer<-')

#' Title
#'
#' @param x x
#' @param ... ...
#'
#' @export
set_column_header_buffer <- function(x, ...) UseMethod('set_column_header_buffer')

#' @export
set_column_header_buffer.rtf_doc <- function(x, top=0, bottom=0, ...) {

  # Check the inputs
  valid <- all(sapply(list(top, bottom), function(x) length(x) == 1 && is.numeric(x) && x%%1==0 && x >= 0))
  assert_that(valid, msg= "Top and bottom values must be positive whole numbers")

  attr(x, 'column_header_buffer') <- c(top=top, bottom=bottom)
  x
}

#' @export
'column_header_buffer<-.rtf_doc' <- function(x, value, ...) {

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
