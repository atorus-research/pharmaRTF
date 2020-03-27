context("rtf-code-generators")
library(huxtable)
library(readr)

test_that("font_table_string adds fonts correctly", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  #defaults
  expect_equal(font_table_string(rtf), "{\\fonttbl\n  {\\f0 Times;}\n  {\\f1 Courier New;}\n}")

  font(rtf) <- "Times New Roman"
  expect_equal(font_table_string(rtf), "{\\fonttbl\n  {\\f0 Times;}\n  {\\f1 Times New Roman;}\n}")

  font(rtf) <- c("Calibri", "Georgia")
  expect_equal(font_table_string(rtf), "{\\fonttbl\n  {\\f0 Times;}\n  {\\f1 Calibri;}\n  {\\f2 Georgia;}\n}")
})

test_that("doc_properties_string populates correctly", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  #defaults
  expect_equal(doc_properties_string(rtf),
               "\\paperw15840\\paperh12240\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt1440\\margb1440\n\\headery720\\footery720\\fs24\n")

  #Twips are 1/1440 inch
  pagesize(rtf) <- c(width = 10)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh12240\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt1440\\margb1440\n\\headery720\\footery720\\fs24\n")
  pagesize(rtf) <- c(height = 10)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt1440\\margb1440\n\\headery720\\footery720\\fs24\n")
  margins(rtf) <- c(top = 2)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt2880\\margb1440\n\\headery720\\footery720\\fs24\n")
  margins(rtf) <- c(bottom = 2)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt2880\\margb2880\n\\headery720\\footery720\\fs24\n")
  margins(rtf) <- c(left = 2)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl2880\\margr1440\\margt2880\\margb2880\n\\headery720\\footery720\\fs24\n")
  margins(rtf) <- c(right = 2)
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl2880\\margr2880\\margt2880\\margb2880\n\\headery720\\footery720\\fs24\n")
  header_height(rtf) <- 1
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl2880\\margr2880\\margt2880\\margb2880\n\\headery1440\\footery720\\fs24\n")
  footer_height(rtf) <- 1
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl2880\\margr2880\\margt2880\\margb2880\n\\headery1440\\footery1440\\fs24\n")
  #font size is font_size * 2
  font_size(rtf) <- 5
  expect_equal(doc_properties_string(rtf),
               "\\paperw14400\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl2880\\margr2880\\margt2880\\margb2880\n\\headery1440\\footery1440\\fs10\n")

})

test_that("header_string lines populates correctly" ,{
  ht <- huxtable(
    column1 = 1:5,

    column2 = letters[1:5]
  )
  rtf1 <- rtf_doc(ht)
  headers1 <- header_string(rtf1)

  headers_l <- list(
    hf_line("Text1"),
    hf_line("Text2", bold = TRUE),
    hf_line("Text3", italic = TRUE),
    hf_line("Text4", font = "Calibri"),
    hf_line("Text5", font_size = 15),
    hf_line(c("Text6", "Text7"), align = "split"),
    hf_line("Text8", align = "right"),
    hf_line("Text9", align = "left")
  )
  rtf2 <- rtf_doc(ht, titles = headers_l)
  headers2 <- header_string(rtf2)

  expect_equal(headers1, read_file("headers1.txt"))





  expect_true(TRUE)
})

# test_that("write_rtf writes an expected rtf_file", {
#   mat <- matrix(
#     c(1:5, letters[1:5], LETTERS[1:5],
#     6:10, letters[6:10],LETTERS[6:10]),
#     byrow = TRUE,
#     nrow=5,
#     ncol = 5
#   )
#   ht <- huxtable(
#     mat
#   )
#   huxtable::set_font(ht, 1, 1:5, "Comic Sans")
#   huxtable::font_size(ht)[,2] <- 15
#   huxtable::bold(ht)[,3] <- TRUE
#   huxtable::italic(ht)[,4] <- TRUE
#   huxtable::col_width(ht)[5] <- 2
#
#   huxtable::align(ht)[1,] <- "left"
#   huxtable::align(ht)[2,] <- "center"
#   huxtable::align(ht)[3,] <- "right"
#   huxtable::valign(ht)[4,] <- "middle"
#   huxtable::row_height(ht)[5] <- 2
#
#   titles <- list(
#     hf_line("A Test RTF Document"),
#     hf_line("Manually checked and used for changes to the package", align = "left")
#   )
#   footers <- list(
#     hf_line("These are footers"),
#     hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
#     hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
#             font_size = 10, italic = TRUE),
#     hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
#     hf_line("FILE_PATH: File source: %s", index = 2)
#   )
#
#   rtf <- rtf_doc(ht, header.rows = 3, titles = titles)
#   #This statement is blocked by issue 85
#   rtf <- add_footnotes(rtf, hf_line("These are footers"),
#                        hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
#                        hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
#                                font_size = 10, italic = TRUE),
#                        hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
#                        hf_line("FILE_PATH: File source: %s", index = 2))
#
#   ignore_cell_padding(rtf) <- TRUE
#   column_header_buffer(rtf) <- c(top = 1, bottom = 2)
#   margins(rtf) <- c(top = 1, bottom = 2, left = 2, right = 0.5)
#   orientation(rtf) <- "portrait"
#   header_height(rtf) <- 1.5
#   footer_height(rtf) <- 2
#   pagesize(rtf) <- c(height = 12, width = 12)
#
#   # write_rtf(rtf)
#
# })




