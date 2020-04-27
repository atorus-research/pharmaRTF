## Auto formatting page numbers ----
#' Return RTF encoding for current page number
#'
#' @param properties Properties for displaying page number information
#'
#' @return String of RTF encoding that will display the current page
#' @noRd
page_num <- function(properties='') {

  # TODO: Add style and font support
  page_str <- sprintf("{%s\\field\\flddirty{\\*\\fldinst{  PAGE   \\\\* MERGEFORMAT }}}", properties)
  page_str
}

#' Return RTF encoding for total page number.
#'
#' @param properties Properties for displaying page number information
#'
#' @return String of the RTF encoding representing total page numbers
#' @noRd
page_total <- function(properties='') {

  tot_str <- sprintf("{%s\\field{\\*\\fldinst{ NUMPAGES}}}", properties)
  tot_str
}

#' Add page number information
#'
#' Adds current and total page number. First %s is current page, second %s is
#' total pages.
#'
#' @param format Format for string replacement
#' @param properties Properties for displaying page number information
#'
#' @return String of RTF encoding that displays the current and total pages.
#' @noRd
add_page_num <- function(format="Page %s of %s", properties='') {

  # Make sure there's only a replacement for current and total pages
  token_ct <- unlist(gregexpr("\\%s", format))
  assert_that(length(token_ct) <= 2,
              msg = "Too many replacement strings - limited to 2 for current page and total pages.")

  # Split out the tokens of the string, apply brackets, and bring them back together
  chunks <- unlist(strsplit(format, "%s"))
  # Build the string to be formatted and incorporate the properties from the line
  fmt_str <- paste(paste0("{", rep(properties, length(chunks)), chunks, "}"), collapse="%s")

  # If the last replacement token was found at the second to last character, then it was not maintained
  # with the string split, so add it back on
  if (token_ct[length(token_ct)] == nchar(format) - 1) fmt_str <- paste(fmt_str, "%s", sep="")

  # Format in the
  page_str <- sprintf(fmt_str, page_num(properties), page_total(properties))
  page_str
}

#' Font table
#'
#' @param doc RTF document
#'
#' @return String of RTF encoding with font information
#' @noRd
font_table_string <- function(doc){
  fonts <- unique(c("Times", font(doc)))
  font_tbl_body <- paste0("  {\\f", seq(0, along = fonts), " ", fonts, ";}", collapse = "\n")
  paste("{\\fonttbl", font_tbl_body , "}", sep = "\n")
}

## Color Table ----
# Not investing in this as the moment so write out a default blank table
#' Create document color table
#'
#' Not currently implemented
#'
#' @param doc RTF document
#'
#' @return String of RTF encoding with color table information
#' @noRd
color_table_string <- function(doc){
  paste('{\\colortbl;;}\n')
}

#' Generate document properties string
#'
#' @param doc RTF document
#'
#' @return String encoding with document property information
#' @noRd
doc_properties_string <- function(doc){

  # Get margins and convert to twips
  mrgs <- sapply(margins(doc), function(x) x*1440)
  # Make margin string
  mrg_str <- sprintf("\\margl%s\\margr%s\\margt%s\\margb%s\n", mrgs['left'], mrgs['right'], mrgs['top'], mrgs['bottom'])

  # Height and width string
  ps <- pagesize(doc)


  # Header and footer heights
  hf_ht <- sprintf("\\headery%s\\footery%s", header_height(doc) * 1440, footer_height(doc) * 1440)

  # Get orientation string
  if (orientation(doc) == 'landscape') {
    ortn <- '\\lndscpsxn\n'
    # If the orientation is landscape, reverse the height and width, effectively flipping 90 degrees
    ht_wd <- sprintf('\\paperw%s\\paperh%s', ps['height'] * 1440, ps['width'] * 1440)
  } else{
    ortn <- ''
    # For portrait, use the values as they were entered
    ht_wd <- sprintf('\\paperw%s\\paperh%s', ps['width'] * 1440, ps['height'] * 1440)
  }

  # Font size
  fs <- sprintf("\\fs%s\n", font_size(doc)*2)

  # Other information
  other <- '\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n'

  paste(ht_wd, other, ortn, mrg_str, hf_ht, fs, sep='')

}

## Header and footer string generation ----
#' Create a single line of RTF header/footnote information
#'
#' @param line A single title/footnote to write
#' @param doc RTF document
#'
#' @return String of RTF encoding for title/footnotes
#' @noRd
hf_line_string <- function(line, doc=NULL) {

  # Placeholders
  ft <- '\\f1' # Font (comes from a list, but using \f0 doesn't seem to work)
  fs <- sprintf("\\fs%s", font_size(doc) * 2) # Font size - no way to set universal document font size, just defaults to 12
                                              # so use the documents set size
  bd <- '' # Bold (On or off - default off)
  it <- '' # Italic (One or off - default off)
  al <- '\\ql\n' # Alignment (Defaults to left \ql - left aligned)
  tabs <- '' # Overwritten if split alignment

  # Read the font information
  # If font is overridden generate the string
  if (!is.na(font(line))) {
    # In huxtable they subtract one because the font list is 0 based, but instead of
    # storing an unused font in the font attribute of the document, I'm just writing out an used font
    # to the font table in the RTF document and matching the index as if it were 1 based.
    ft <- sprintf("\\f%s", match(font(line), font(doc)))
  }

  # If font size is overridden generate the string
  if (!is.na(font_size(line))) {
    fs <- sprintf("\\fs%s", font_size(line)*2)
  }

  # Styling
  # use the bold string if on
  if (bold(line)) bd <- "\\b"
  # Use the italics string if on
  if (italic(line)) it <- "\\i"

  # Concatenate all of the text level properties
  properties <- paste(ft, fs, bd, it, ' ', sep='')

  # Alignment
  if      (align(line) == 'center') al <- '\\qc\n'
  else if (align(line) == 'right')  al <- '\\qr\n'
  # Split will align left and designate tab locations, where the right most is flush right
  else if (align(line) == 'split')  {
    al <- "\\ql\\tx7245\\tqr\\tx12960\n"
    tabs <- '\\pmartabqr \n'
  }
  txt_string <- sapply(line$text, format_text_string, properties = properties, USE.NAMES=FALSE)

  # Patch
  if (length(txt_string) > 1) {
    txt_string <- paste(txt_string[1], tabs, txt_string[2], sep='')
  }

  paste(al, txt_string, sep='')

}

#' General function to write the header or the footer
#'
#' @param doc doc RTF document
#' @param type 'header' of 'footer'
#'
#' @return String RTF encoding with the header/footnote information
#' @noRd
hf_string <- function(doc, type=NULL) {
  # Get a character vector of the formatted RTF string
  lines <- sapply(order_lines(doc[[type]]), hf_line_string, doc=doc)

  # Piece together each of the lines
  body <- paste(lines, collapse="\n\\par")

  # Get the opening command word for the header or footer
  if (type == 'titles') command <- '\\header\n'
  else if (type == 'footnotes') command <- '\\footer\n'

  # Generate the final string
  if (type == "titles") {
    # If generating titles then take the headers of the table
    paste('{', command, body, '\n\\par\n', get_column_headers(doc), '\n}', sep='')
  } else {
    paste('{', command, body, '\\par\n}', sep='')
  }
}

#' Create the header string
#'
#' @param doc RTF document
#'
#' @return String RTF encoding with the header information
#' @noRd
header_string <- function(doc) {
  hf_string(doc, type='titles')
}

#' Create the footer string
#'
#' @param doc RTF document
#'
#' @return String RTF encoding with the footer information
#' @noRd
footer_string <- function(doc) {
  hf_string(doc, type='footnotes')
}

#' Write RTF document
#'
#' Writes the RTF document to a specified file.
#'
#' @param doc The RTF document to be written.
#' @param file A character string naming a file open for writing.
#'
#' @examples
#' \dontrun{
#' ## Create and write RTF document
#' #' ht <- huxtable::huxtable(
#'  column1 = 1:5,
#'  column2 = letters[1:5]
#' )
#' rtf <- rtf_doc(ht)
#'
#' write_rtf(rtf) #writes a table with no header/footnotes to 'test.rtf'
#' }
#'
#' @seealso \url{http://www.biblioscape.com/rtf15_spec.htm},
#'   \url{http://latex2rtf.sourceforge.net/rtfspec_7.html#rtfspec_tabledef}
#' @export
write_rtf <- function(doc, file='test.rtf') {

  # Write to the specified file
  sink(file)
  tryCatch({
    # RTF Header line
    cat("{\\rtf1\\ansi\\deff1\n")
    # Fot table
    cat(font_table_string(doc))
    # Color table
    cat(color_table_string(doc))
    cat("\n\n\n")
    # Document properties
    cat(doc_properties_string(doc))
    cat("\n\n\n")
    # Titles
    cat(header_string(doc))
    cat("\n")
    # Footnotes
    cat(footer_string(doc))
    # Table content
    cat(get_table_body(doc))
    cat("\n}")
  },
    error = function(err) {stop(paste(err))},
    finally = {sink()}
  )
}
