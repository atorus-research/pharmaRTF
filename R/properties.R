# Property extraction of different rtf_doc and child objects ####

## FONT SPECIFIC GETTERS AND SETTERS (special because they work across multiple levels)

## Fonts (getters) ----
# S3 Generic
#' Return or set font
#'
#' @description
#' These property function modify or return the fonts of an rtf_doc object or
#' individual titles/footnotes objects of the hf_line class.
#'
#' When used on an rtf_doc object to retrieve fonts, the distinct set of fonts
#' of all objects contained within the rtf_doc are returned. When used on an
#' rtf_doc to set fonts, the default font for the RTF document is set.
#'
#' When used on titles/footnotes (hf_line objects), the font is either returned
#' of set for that individual line.
#'
#' @param x \code{rtf_doc} object, the table of a \code{rtf_doc} object, or a
#'   \code{hf_line} object
#' @param ... Additional arguments passed to method dispatch
#'
#' @return For \code{font()}, the font attribute of the object in the case of
#'   \code{hf_line} and the table, or each unique font in the table, titles,
#'   footnotes, and the overall document in the case of \code{rtf_doc}. For
#'   \code{set_font()} and \code{`font<-`()}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("text", font = "Comic Sans")))
#'
#' pharmaRTF::font(rtf)
#' # Returns "Courier New" "Comic Sans"
#'
#' pharmaRTF::font(rtf) <- "Times"
#' # Returns "Times" "Comic Sans"
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
#' @description
#' These property function modify or return the font sizes of an rtf_doc object
#' or individual titles/footnotes objects of the hf_line class.
#'
#' When used on an rtf_doc object to retrieve font sizes, the document level
#' default font size within the rtf_doc is returned. When used on an rtf_doc to
#' set fonts, the default font size for the RTF document is set.
#'
#' When used on titles/footnotes (hf_line objects), the font size is either
#' returned of set for that individual line.
#'
#' @param x \code{rtf_doc} object or \code{hf_line} object.
#' @param ... Additonal arguments passed to method dispatch.
#'
#' @return For \code{font_size()}, the font_size attribute of the supplied
#'   \code{rtf_doc} or \code{hf_line}. For \code{`font_size<-`()} and
#'   \code{set_font_size}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle", font_size = 15)))
#'
#' pharmaRTF::font_size(rtf)
#' # Returns 12
#'
#' pharmaRTF::font_size(rtf) <- 14
#'
#' pharmaRTF::font_size(rtf$titles[[1]])
#' # Returns 15
#' pharmaRTF::font_size(rtf)
#' # Returns 14
#'
#'
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
#' @param x \code{rtf_doc} object or a \code{hf_line} object.
#' @param value A numeric value for font size in points.
#'
#' @export
#' @rdname font_size
'font_size<-' <- function(x, value) UseMethod('font_size<-')

#' @param x \code{rtf_doc} object or a \code{hf_line} object.
#' @param value A numeric value for font size in points.
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
#' Return or set text alignment
#'
#' @description
#' These property functions modify or return the text alignment in a \code{hf_line}
#' object. Supported options are: 'left', 'right', 'center', and 'split'.
#'
#' An alignment option of 'split' requires that two text elements are provided
#' in the titles/footnotes \code{hf_line} object. The first text element will
#' be aligned on the left and the second on the right.
#'
#' @param x A \code{hf_line} object
#' @param ... Additional arguments passed to method dispatch.
#'
#' @return For \code{align()}, the alignment of the supplied \code{hf_line} object.
#'   For \code{set_align()} and \code{`align<-`()}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' pharmaRTF::align(rtf$titles[[1]])
#' # Returns "center"
#'
#' pharmaRTF::align(rtf$titles[[1]]) <- "left"
#' # Sets alignment to 'left'
#'
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
#' These property functions modify or return the bold attribute of a
#' \code{hf_line} object.
#'
#' @param x A \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{bold()}, the bold attribute of the supplied \code{hf_line}
#'   object. For \code{`bold<-`()} and \code{set_bold()}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' pharmaRTF::bold(rtf$titles[[1]])
#' # Returns FALSE
#'
#' pharmaRTF::bold(rtf$titles[[1]]) <- TRUE
#' # Sets bold to TRUE
#'
#' @export
#' @rdname bold
bold <- function(x, ...) UseMethod('bold')

#' @export
bold.hf_line <- function(x, ...) {
  attr(x, 'bold')
}

## Bold (setters) ----
#' @param x A \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname bold
'bold<-' <- function(x, value) UseMethod('bold<-')

#' @param x A \code{hf_line} object
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
#' These functions modify or return the italics attribute of a \code{hf_line}
#' object. The italic attribute takes on a logical value of TRUE or FALSE, where
#' TRUE italicizes the text of the line.
#'
#' @param x A \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{italic()}, the italic attribute of the supplied
#'   \code{hf_line}. Fot \code{`italic<-()`} and \code{set_italic()} the
#'   modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' pharmaRTF::italic(rtf$titles[[1]])
#' # Returns FALSE
#'
#' pharmaRTF::italic(rtf$titles[[1]]) <- TRUE
#' # Sets italic to TRUE
#'
#' @export
#' @rdname italic
italic <- function(x, ...) UseMethod('italic')

#' @export
italic.hf_line <- function(x, ...) {
  attr(x, 'italic')
}


## Italic (setters) ----
#' @param x A \code{hf_line} object
#' @param value A logical vector to set the value of the bold attribute
#'
#' @export
#' @rdname italic
'italic<-' <- function(x, value) UseMethod('italic<-')

#' @param x A \code{hf_line} object
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
#' These functions modify or return the fonts of a \code{rtf_doc} object.
#' \code{text()} will always return a vector of length 2. If the text is only
#' of length one an empty string will be appended.
#'
#' @param x A \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{text()}, the text vector of the supplied \code{hf_line}
#'   object. For \code{`text<-()`} and \code{set_text()}, the modfied object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' text(rtf$titles[[1]])
#' # Returns "aTitle" ""
#'
#' text(rtf$titles[[1]]) <- "aDifferentTitle"
#' # Sets titles to "aDifferentTitle"
#'
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

#' @param x A \code{hf_line} object
#' @param value A character vector of length 0, 1, or 2 to set the text value
#'   of a \code{hf_line} object.
#'
#' @export
#' @rdname text
'text<-' <- function(x, value) UseMethod('text<-')

#' @param x A \code{hf_line} object
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
#' These functions modify or return the index of a \code{hf_line} object. The
#' index sets the order in which a title will appear. The default index value in
#' NULL, and NULL values will sort behind any populated index Indices can be any
#' numeric value as long as they are not duplicated.
#'
#' @param x A \code{hf_line} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{index()}, the index attribute of the supplied
#'   \code{hf_line} object. For \code{`index<-()`} and \code{set_index()}, the
#'   modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' index(rtf$titles[[1]])
#' # Returns NULL
#'
#' index(rtf$titles[[1]]) <- 2
#' # Sets index of first titles to 2
#'
#' @export
#' @rdname index
index <- function(x, ...) UseMethod('index')

#' @export
index.hf_line <- function(x, ...) {
  attr(x, 'index')
}

## Index (setters) ----
#' @param x A \code{hf_line} object
#' @param value Numeric value to order index
#'
#' @export
#' @rdname index
'index<-' <- function(x, value) UseMethod('index<-')

#' @param x A \code{hf_line} object
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
#' Return or set margins
#'
#' These functions return or set the margin attribute of a \code{rtf_doc}
#' object. These are stored as a named vector. Names should be \code{top},
#' \code{bottom}, \code{left}, and \code{right}. Margins are measured in inches.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{margin()}, a named vector of the margin attribute of the
#'   supplied \code{rtf_doc}. For \code{`margin<-()`} and \code{set_margin()}
#'   the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' margins(rtf)
#' # Returns c(top = 1, bottom = 1, left = 1, right = 1)
#'
#' margins(rtf) <- c(top = 2)
#' # Sets top margin to 2
#'
#' @export
#' @rdname margins
margins <- function(x, ...) UseMethod('margins')

#' @export
margins.rtf_doc <- function(x, ...) {
  attr(x, 'margins')
}

## Margins (setters) ----
#' @param x A \code{rtf_doc} object
#' @param value A named list or vector detailing the
#'
#' @export
#' @rdname margins
'margins<-' <- function(x, value) UseMethod('margins<-')

#' @param x A \code{rtf_doc} object
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
#' Return or set orientation
#'
#' These functions modify or return the orientation attribute. Options are
#' landscape or portrait.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{orientation()}, the orientation attribute of the supplied
#'   \code{rtf_doc} object. For \code{`orientation<-()`} and
#'   \code{set_orientation()} the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' orientation(rtf)
#' # Returns landscape
#'
#' orientation(rtf) <- "portrait"
#' # Sets orientation to portrait
#'
#' @export
#' @rdname orientation
orientation <- function(x, ...) UseMethod('orientation')

#' @export
orientation.rtf_doc <- function(x, ...) {
  attr(x, 'orientation')
}

## Orientation (setters) ----

#' @param x A \code{rtf_doc} object
#' @param value A character vector of either 'landscape' or 'portrait'
#'
#' @export
#' @rdname orientation
'orientation<-' <- function(x, value) UseMethod('orientation<-')

#' @param x A \code{rtf_doc} object
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
#' Return or set header/footer height
#'
#' These functions modify or return the header_height/footer_height attribute
#' of a rtf_doc object. The header/footer height is the default amount of
#' space allocated to the header/footer from the margin. If the content of the
#' header/footer exceeds this amount of space, it will be expanded.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{header_height()}/\code{footer_height()}, the
#'   header_height/footer_height attribute of thesupplied \code{rtf_doc}
#'   object. For \code{`header_height<-()`}/ \code{`footer_height<-`()} and
#'   \code{set_header_height()}/ \code{set_footer_height()}, the modified
#'   object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' header_height(rtf)
#' # Returns 0.5, the default
#'
#' footer_height(rtf) <- 1
#' # Sets footer_height to 1
#'
#' @export
#' @rdname header_footer_height
header_height <- function(x, ...) UseMethod('header_height')

#' @export
header_height.rtf_doc <- function(x, ...) {
  attr(x, 'header_height')
}

#' @param x A \code{rtf_doc} object
#' @param value A numeric value to set the header_height/footer_height
#'
#' @export
#' @rdname header_footer_height
'header_height<-' <- function(x, value) UseMethod('header_height<-')


#' @param x A \code{rtf_doc} object
#' @param value A numeric value to set the header_height/footer_height
#'
#' @export
#' @rdname header_footer_height
set_header_height <- function(x, value) UseMethod('header_height<-')

#' @export
'header_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'header_height') <- value
  x
}

## Footer Height ----
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @export
#' @rdname header_footer_height
footer_height <- function(x, ...) UseMethod('footer_height')

#' @export
footer_height.rtf_doc <- function(x, ...) {
  attr(x, 'footer_height')
}

## Footer height (setters) ----
#' @param x A \code{rtf_doc} object
#' @param value A numeric value to set the header_height/footer_height
#'
#' @export
#' @rdname header_footer_height
'footer_height<-' <- function(x, value) UseMethod('footer_height<-')

#' @param x A \code{rtf_doc} object
#' @param value A numeric value to set the header_height/footer_height
#'
#' @export
#' @rdname header_footer_height
set_footer_height <- function(x, value) UseMethod('footer_height<-')

#' @export
'footer_height<-.rtf_doc' <- function(x, value) {
  # Make sure the value is valid
  assert_that(is.numeric(value))

  attr(x, 'footer_height') <- value
  x
}

## Page Size (getters) ----
#' Return or set pagesize
#'
#' These functions modify or return the \code{pagesize} attribute of a
#' \code{rtf_doc} object. Stored as a named vector with \code{height} and
#' \code{width} names.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{pagesize()}, the pagesize attribute of the \code{rtf_doc}
#'   object. For \code{`pagesize<-`()} and \code{set_pagesize()}, the modified
#'   object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' pagesize(rtf)
#' # Returns c(height = 8.5, width = 11.0)
#'
#' pagesize(rtf) <- c(height = 12)
#' # Sets height of page to 12 inches
#'
#' @export
#' @rdname pagesize
pagesize <- function(x, ...) UseMethod('pagesize')

#' @export
pagesize.rtf_doc <- function(x, ...) {
  attr(x, 'pagesize')
}

## Page size(setters) ----
#' @param x A \code{rtf_doc} object
#' @param value A named numeric vector with the names \code{height} and
#'   \code{width}.
#'
#' @export
#' @rdname pagesize
'pagesize<-' <- function(x, value) UseMethod('pagesize<-')

#' @param x A \code{rtf_doc} object
#' @param value A named numeric vector with the names \code{height} and
#'   \code{width}.
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
#' Return or set the header_rows
#'
#' @description
#' These functions modify or return the header_rows attribute of a rtf_doc
#' object. Only required and valid when the rtf_doc table object is a
#' huxtable.
#'
#' The header rows control the number of rows taken from a huxtable table into
#' the header of the document as the column header. When pulled into the
#' headers, these rows are repeated on each page.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{header_rows()}, the header_rows attribute of the
#'   \code{rtf_doc} object. For \code{`header_rows<-`()} and
#'   \code{set_header_rows()}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' header_rows(rtf)
#' # This is a wrapper for header_rows(x$table)
#' header_rows(rtf$table)
#' # Both of these return 1, the default
#'
#' header_rows(rtf$table) <- 0
#' # Sets reader_rows to 0
#'
#' @export
#' @rdname header_rows
header_rows <- function(x, ...) UseMethod('header_rows')

#' @export
header_rows.rtf_doc <- function(x, ...) {
  header_rows(x$table)
}

#' @export
header_rows.huxtable <- function(x, ...) {
  attr(x, 'header_rows')
}

#' @export
header_rows.gt_tbl <- function(x, ...) {
  stop('GT tables do not require header rows to be set')
}

## Header rows (setters) ----
#' @param x A \code{rtf_doc} object
#' @param value A numeric value to change the header_rows attribute.
#'
#' @export
#' @rdname header_rows
'header_rows<-' <- function(x, value) UseMethod('header_rows<-')

#' @param x A \code{rtf_doc} object
#' @param value A numeric value to change the header_rows attribute.
#'
#' @export
#' @rdname header_rows
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
  attr(x, 'header_rows') <- value
  x
}

#' @export
'header_rows<-.gt_tbl' <- function(x, value) {
  stop('GT tables do not require header rows to be set')
}

## Ignore Cell Padding (getters) ----
#' Return or set ignore_cell_padding
#'
#' These functions modify and return the ignore_cell_padding attribute of a
#' \code{rtf_doc} object. By default, the huxtable package will pad rows of a
#' table. This attribute will remove those default settings – which allow the
#' cells to have a smaller amount of padding than setting the cell padding to
#' 0. See the Details section for a more thorough description of the
#' implementation.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @details
#' Cell padding in RTF code has multiple command words associated with it.
#' Huxtable uses the command word \\clpad<t,b,l,r>N to control the cell padding.
#' This command word is additionally controlled by the command word
#' \\clpadf<t,b,l,r>N. There are two possible values for  N in \\clpadf<t,b,l,r>N:
#'
#' \itemize{
#'   \item{0: Null. This ignores \\clpad<t,b,l,r> in favor of \\trgaph (Word 97
#'     style cell padding).}
#'   \item{3: Twips}
#' }
#'
#' The ignore_cell_padding function toggles the \\clpadf<t,b,l,r>N command words
#' in the RTF document to 0 instead of 3. By using Word 97 style cell padding,
#' the minimum amount of space is closer than using the 0 twips setting when the
#' RTF is rendered inside Word. This effectively closes the gap between rows,
#' which may be a desirable appearance in some outputs.
#'
#' More information on these RTF settings can be found
#' \href{http://www.biblioscape.com/rtf15_spec.htm}{here}.
#'
#'
#' @return For \code{ignore_cell_padding()}, the ignore_cell_padding attribute
#'   of the supplied \code{rtf_doc} object. For
#'   \code{`ignore_cell_padding<-`()} and \code{set_ignore_cell_padding()}, the
#'   modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' ignore_cell_padding(rtf)
#' # Returns FALSE, the default
#'
#' ignore_cell_padding(rtf) <- TRUE
#' # Sets ignore_cell_padding to TRUE
#'
#' @export
#' @rdname ignore_cell_padding
ignore_cell_padding <- function(x, ...) UseMethod('ignore_cell_padding')

#' @export
ignore_cell_padding.rtf_doc <- function(x, ...) {
  attr(x, 'ignore_cell_padding')
}

## Ignore Cell Padding (setters) ----
#' @param x A \code{rtf_doc} object
#' @param value A logical value to set the attribute
#'
#' @export
#' @rdname ignore_cell_padding
'ignore_cell_padding<-' <- function(x, value) UseMethod('ignore_cell_padding<-')

#' @param x A \code{rtf_doc} object
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
#' Return or set column_header_buffer attributes
#'
#' These property functions modify and return the column header buffers of a
#' \code{rtf_doc}. This attribute adds rows to the top or bottom of the table
#' column headers to pad it from the titles above or the table below.
#'
#' @param x A \code{rtf_doc} object
#' @param ... Additonal arguments passed to method dispatch
#'
#' @return For \code{column_header_buffer}, the column_header_buffer attribute
#'   of the supplied \code{rtf_doc}. For \code{`column_header_buffer<-`()} and
#'   \code{set_column_header_buffer()}, the modified object.
#'
#' @examples
#' library(huxtable)
#' ht <- huxtable(
#'   column1 = 1:5,
#'   column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht, list(hf_line("aTitle")))
#'
#' column_header_buffer(rtf)
#' # Returns c(top = 0, bottom = 0), the default
#'
#' column_header_buffer(rtf) <- c(bottom = 1)
#' # Sets the bottom column_header_buffer to 1
#'
#' @export
#' @rdname column_header_buffer
column_header_buffer <- function(x, ...) UseMethod('column_header_buffer')

#' @export
column_header_buffer.rtf_doc <- function(x, ...) {
  attr(x, 'column_header_buffer')
}

#' @param x A \code{rtf_doc} object
#' @param value A named vector detailing the top and bottom buffer.
#'
#' @export
#' @rdname column_header_buffer
'column_header_buffer<-' <- function(x, value) UseMethod('column_header_buffer<-')

#' @param x A \code{rtf_doc} object
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
