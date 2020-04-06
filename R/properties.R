# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
#' Return or set font
#'
#' These functions modify and return the fonts of a \code{rtf_doc} object and
#' associated items. To set the font attribute of a table you should use the
#' functions assosiated with that package and modify the table directly.
#'
#' @param x \code{rtf_doc} object, the table of a \code{rtf_doc} object, or a
#'   \code{hf_line} object
#' @param ... Additional arguments passed to method dispatch
#'
#' @return For \code{font()}, the font attribute of the object in the case of
#'   \code{rtf_doc}, or each unique font in the table, titles, footnotes, and the
#'   overall document in the case of \code{rtf_doc}. For \code{set_font()} and
#'   \code{`font<-`()}, the modified object.
#'
#' @export
#' @rdname font
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
#' @param x \code{rtf_doc} object, the table of a \code{rtf_doc} object, or a
#'   \code{hf_line} object
#' @param value A string representing a font
#'
#' @export
#' @rdname font
'font<-' <- function(x, value) UseMethod('font<-')

#' @param x \code{rtf_doc} object, the table of a \code{rtf_doc} object, or a
#'   \code{hf_line} object
#' @param value A string representing a font
#'
#' @export
#' @rdname font
set_font <- function(x, value) UseMethod('font<-')

#' @export
'font<-.hf_line' <- function(x, value) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

#' @export
'font<-.rtf_doc' <- function(x, value) {
  assert_that(is.character(value))
  attr(x, 'font') <- value
  x
}

## Font size (getters) ----
#' Return or set font size
#'
#' These functions modify and return the font sizes of a \code{rtf_doc} object
#' or a \code{hf_line} object. This attribute measures the font size in half
#' points. A font_size of 24 will result in a 12 point font.
#'
#' @param x \code{rtf_doc} object or \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return The font_size attribute of the supplied \code{rtf_doc} or \code{hf_line}
#' @export
#' @rdname font_size
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
#' @param x \code{rtf_doc} object or a \code{hf_line} object
#' @param value A numeric value for font size in half points. A font_size of 24
#'   will result in a 12 point font.
#'
#' @export
#' @rdname font_size
'font_size<-' <- function(x, value) UseMethod('font_size<-')

#' @param x \code{rtf_doc} object or a \code{hf_line} object
#' @param value A numeric value for font size in half points. A font_size of 24
#'   will result in a 12 point font.
#'
#' @export
#' @rdname font_size
set_font_size <- function(x, value) UseMethod('font_size<-')

#' @export
'font_size<-.hf_line' <- function(x, value) {
  assert_that(is.numeric(value) && value %% 0.5 == 0,
              msg = "Font size must be numeric and divisible by .5")
  attr(x, 'font_size') <- value
  x
}

#' @export
'font_size<-.rtf_doc' <- function(x, value) {
  assert_that(is.numeric(value) && value %% 0.5 == 0,
              msg = "Font size must be numeric and divisible by .5")
  attr(x, 'font_size') <- value
  x
}

## HF_LINE PROPERTIES (and attributes that spread to rtf_doc level) ####

## Alignment (getters) ----
#' Return and set text alignment
#'
#' These functions modify and return the text alignment in a \code{hf_line}
#' object. Supported options are: 'left', 'right', 'center', and 'split'.
#'
#' @param x \code{hf_line} object
#' @param ... Additional arguments passed to method dispatch.
#'
#' @return For \code{align()}, the alignment of the supplied \code{hf_line} object.
#'   For \code{set_align()} and \code{`align<-`()}, the modified object.
#' @export
#' @rdname align
align <- function(x, ...) UseMethod('align')

#' @export
align.hf_line <- function(x, ...) {
  attr(x, 'align')
}

## Alignment (setters) ----
#' @param x \code{hf_line} object
#' @param value A string representing the alignment.
#'
#' @export
#' @rdname align
'align<-' <- function(x, value) UseMethod('align<-')

#' @param x \code{hf_line} object
#' @param value A string representing the alignment.
#'
#' @export
#' @rdname align
set_align <- function(x, value) UseMethod('align<-')

#' @export
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
#' Return or set bold
#'
#' These functions modify and return the bold attribute of a \code{hf_line} object.
#'
#' @param x \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{bold()}, the bold attribute of the supplied \code{hf_line}
#'   object. For \code{`bold<-`()} and \code{set_bold()}, the modified object.
#' @export
#' @rdname bold
bold <- function(x, ...) UseMethod('bold')

#' @export
bold.hf_line <- function(x, ...) {
  attr(x, 'bold')
}

## Bold (setters) ----
#' @param x \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname bold
'bold<-' <- function(x, value) UseMethod('bold<-')

#' @param x \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname bold
set_bold <- function(x, value) UseMethod('bold<-')

#' @export
'bold<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'bold') <- value
  x
}

## Italic (getters) ----
#' Return or set italics
#'
#' These functions modify and return the italics attribute of a \code{hf_line}
#' object
#'
#' @param x \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return The italic attribute of the supplied \code{hf_line}
#' @export
#' @rdname italic
italic <- function(x, ...) UseMethod('italic')

#' @export
italic.hf_line <- function(x, ...) {
  attr(x, 'italic')
}


## Italic (setters) ----
#' @param x \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname italic
'italic<-' <- function(x, value) UseMethod('italic<-')

#' @param x \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname italic
set_italic <- function(x, value) UseMethod('italic<-')

#' @export
'italic<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'italic') <- value
  x
}

## Text (getter) ----
#' Return or set text
#'
#' These functions modify and return the fonts of a \code{rtf_doc} object.
#' \code{font()} will always return a vector of length 2. If the text is only
#' of length one an empty string will be appended.
#'
#' @param x \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return The text vector of the supplied \code{hf_line} object.
#' @export
#' @rdname text
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

#' @param x \code{hf_line} object
#' @param value A character vector of length 0, 1, or 2 to set the text value
#'   of a \code{hf_line} object.
#'
#' @export
#' @rdname text
'text<-' <- function(x, value) UseMethod('text<-')

#' @param x \code{hf_line} object
#' @param value A character vector of length 0, 1, or 2 to set the text value
#'   of a \code{hf_line} object.
#'
#' @export
#' @rdname text
set_text <- function(x, value) UseMethod('text<-')

#' @export
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
#' Return or set index
#'
#' These functions modify and return the index of a \code{hf_line} object.
#'
#' @param x \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return return
#' @export
#' @rdname index
index <- function(x, ...) UseMethod('index')

#' @export
index.hf_line <- function(x, ...) {
  attr(x, 'index')
}

## Index (setters) ----
#' @param x \code{hf_line} object
#' @param value Numeric value to order index
#'
#' @export
#' @rdname index
'index<-' <- function(x, value) UseMethod('index<-')

#' @param x \code{hf_line} object
#' @param value Numeric value to order index
#'
#' @export
#' @rdname index
set_index <- function(x, value) UseMethod('index<-')

#' @export
'index<-.hf_line' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.numeric(value) | is.null(value))

  attr(x, 'index') <- value
  x
}

## DOCUMENT PROPERTIES ####

## Margins (getters) ----
#' Return and set margins
#'
#' These functions return and set the margin attribute of a \code{rtf_doc}
#' object. Names should be top, bottom, left, and right. Margins are measured
#' in inches.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return A named vector of the margin attribute of the supplied \code{rtf_doc}
#' @export
#' @rdname margins
margins <- function(x, ...) UseMethod('margins')

#' @export
margins.rtf_doc <- function(x, ...) {
  attr(x, 'margins')
}

## Margins (setters) ----
#' @param x \code{rtf_doc} object
#' @param value A named list or vector detailing the
#'
#' @export
#' @rdname margins
'margins<-' <- function(x, value) UseMethod('margins<-')

#' @param x \code{rtf_doc} object
#' @param value A named list or vector detailing the
#'
#' @export
#' @rdname margins
set_margins <- function(x, value) UseMethod('margins<-')

#' @export
'margins<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Must supply a named vector
  assert_that(!is.null(names(values)),
              msg="A named vector must be provided with the a combination of the names `top`, `bottom`, `left`, and `right`")

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
#' Return and set orientation
#'
#' These functions modify and return the orientation attribute. Options are
#' landscape or portrait.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return Orientation attribute of the supplied \code{rtf_doc} object
#'
#' @export
#' @rdname orientation
orientation <- function(x, ...) UseMethod('orientation')

#' @export
orientation.rtf_doc <- function(x, ...) {
  attr(x, 'orientation')
}

## Orientation (setters) ----

#' @param x \code{rtf_doc} object
#' @param value A character vector of either 'landscape' or 'portrait'
#'
#' @export
#' @rdname orientation
'orientation<-' <- function(x, value) UseMethod('orientation<-')

#' @param x \code{rtf_doc} object
#' @param value A character vector of either 'landscape' or 'portrait'
#'
#' @export
#' @rdname orientation
set_orientation <- function(x, value) UseMethod('orientation<-')

#' @export
'orientation<-.rtf_doc' <- function(x, value = c('landscape', 'portrait')) {
  # Make sure the value is valid
  value <- match.arg(value)

  # Check that argument is valid
  assert_that(is.character(value))

  attr(x, 'orientation') <- value
  x
}

## Header height (getters)----
#' Return and set header height
#'
#' These functions modify and return the header_height attribute of a \code{rtf_doc}
#' object
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return header_height attribute of the supplied \code{rtf_doc} object
#' @export
#' @rdname header_height
header_height <- function(x, ...) UseMethod('header_height')

#' @export
header_height.rtf_doc <- function(x, ...) {
  attr(x, 'header_height')
}

#' @param x \code{rtf_doc} object
#' @param value A numeric value to set the header_height
#'
#' @export
#' @rdname header_height
'header_height<-' <- function(x, value) UseMethod('header_height<-')


#' @param x \code{rtf_doc} object
#' @param value A numeric value to set the header_height
#'
#' @export
#' @rdname header_height
set_header_height <- function(x, value) UseMethod('header_height<-')

#' @export
'header_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'header_height') <- value
  x
}

## Footer Height ----
#' Return and set footer height
#'
#' These functions modify and return the footer_height attribute of a \code{rtf_doc}
#' object.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return header_height attribute of the supplied \code{rtf_doc} object
#' @export
#' @rdname footer_height
footer_height <- function(x, ...) UseMethod('footer_height')

#' @export
footer_height.rtf_doc <- function(x, ...) {
  attr(x, 'footer_height')
}

## Footer height (setters) ----
#' @param x \code{rtf_doc} object
#' @param value A numeric value to set the footer_height
#'
#' @export
#' @rdname footer_height
'footer_height<-' <- function(x, value) UseMethod('footer_height<-')

#' @param x \code{rtf_doc} object
#' @param value A numeric value to set the footer_height
#'
#' @export
#' @rdname footer_height
set_footer_height <- function(x, value) UseMethod('footer_height<-')

#' @export
'footer_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'footer_height') <- value
  x
}

## Page Size (getters) ----
#' Return and set pagesize
#'
#' These functions modify and return the pagesize attribute of a \code{rtf_doc}
#' object. Stored as a named vector with height and width names.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return return
#'
#' @export
#' @rdname pagesize
pagesize <- function(x, ...) UseMethod('pagesize')

#' @export
pagesize.rtf_doc <- function(x, ...) {
  attr(x, 'pagesize')
}

## Page size(setters) ----
#' @param x \code{rtf_doc} object
#' @param value A named numeric vector with the names height and width.
#'
#' @export
#' @rdname pagesize
'pagesize<-' <- function(x, value) UseMethod('pagesize<-')

#' @param x \code{rtf_doc} object
#' @param value A named numeric vector with the names height and width.
#'
#' @export
#' @rdname pagesize
set_pagesize <- function(x, value) UseMethod('pagesize<-')

#' @export
'pagesize<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Must supply a named vector
  assert_that(!is.null(names(values)), msg="A named vector must be provided with the names `height`, `width`, or both")

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
#' Return and set the header.rows
#'
#' These functions modify and return the header.rows attribute of a \code{rtf_doc}
#' object.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return return
#' @export
#' @rdname header.rows
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
#' @param x \code{rtf_doc} object
#' @param value A numeric value to change the header.rows attribute.
#'
#' @export
#' @rdname header.rows
'header_rows<-' <- function(x, value) UseMethod('header_rows<-')

#' @param x \code{rtf_doc} object
#' @param value A numeric value to change the header.rows attribute.
#'
#' @export
#' @rdname header.rows
set_header_rows <- function(x, value) UseMethod('header_rows<-')

#' @export
'header_rows<-.rtf_doc' <- function(x, value) {

  header_rows(x$table) <- value
  x
}

#' @export
'header_rows<-.huxtable' <- function(x, value) {
  # Must be a number
  assert_that(is.numeric(value) && (value %% 1 == 0) && (value >= 0), msg='Header rows must be a positive whole number')

  # Set the attribute
  attr(x, 'header.rows') <- value
  x
}

#' @export
'header_rows<-.gt_tbl' <- function(x, value) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
#' Return and set ignore_cell_padding
#'
#' These functions modify and return the ignore_cell_padding attribute of a
#' \code{rtf_doc} object. By default, the huxtable package will pad rows of a
#' table. This attribute will remove the padding.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return The ignore_cell_padding attribute of the supplied \code{rtf_doc}
#'   object
#'
#' @export
#' @rdname ignore_cell_padding
ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding')

#' @export
ignore_cell_padding.rtf_doc <- function(x, ...) {
  attr(x, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
#' @param x \code{rtf_doc} object
#' @param value A logical value to set the attribute
#'
#' @export
#' @rdname ignore_cell_padding
'ignore_cell_padding<-' <- function(x, value) UseMethod('ignore_cell_padding<-')

#' @param x \code{rtf_doc} object
#' @param value A logical value to set the attribute
#'
#' @export
#' @rdname ignore_cell_padding
set_ignore_cell_padding <- function(x, value) UseMethod('ignore_cell_padding<-')

#' @export
'ignore_cell_padding<-.rtf_doc' <- function(x, value) {
  # Check that argument is valid
  assert_that(is.logical(value))

  attr(x, 'ignore_cell_padding') <- value
  x
}

## Column header buffer (getter)
#' Return and set column_header_buffer attributes
#'
#' These functions modify and return the column header buffers of a
#' \code{rtf_doc}. This attribute adds rows to the top or bottom of a header
#' to pad it from the titles above or the table below.
#'
#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return The column_header_buffer attribute of the supplied \code{rtf_doc}
#' @export
#' @rdname column_header_buffer
column_header_buffer <- function(x, ...) UseMethod('column_header_buffer')

#' @export
column_header_buffer.rtf_doc <- function(x, ...) {
  attr(x, 'column_header_buffer')
}

#' @param x \code{rtf_doc} object
#' @param value A named vector detailing the top and bottom buffer.
#'
#' @export
#' @rdname column_header_buffer
'column_header_buffer<-' <- function(x, value) UseMethod('column_header_buffer<-')

#' @param x \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch. Should include
#'   argument top and bottom with numeric elements.
#'
#' @export
#' @rdname column_header_buffer
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
'column_header_buffer<-.rtf_doc' <- function(x, value) {

  values <- unlist(value)

  # Must supply a named vector
  assert_that(!is.null(names(values)), msg="A named vector must be provided with the names `top`, `bottom`, or both")


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
