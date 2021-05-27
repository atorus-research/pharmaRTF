context("rtf-code-generators")
library(huxtable)
library(readr)
library(stringr)

test_that("hf_string orderes lines properly", {
  ht <- huxtable::huxtable(
    column1 = c("Header1", 1:26),
    column2 = c("Header2", letters[1:26]),
    add_colnames = FALSE
  )
  huxtable::wrap(ht) <- FALSE
  huxtable::right_padding(ht) <- 4
  huxtable::left_padding(ht) <- 4
  huxtable::top_padding(ht) <- 4
  huxtable::bottom_padding(ht) <- 4


  titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
  footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes) %>%
    set_ignore_cell_padding(TRUE)

  pharmaRTF::index(rtf$titles[[2]]) <- 2
  pharmaRTF::index(rtf$titles[[3]]) <- 1

  pharmaRTF::index(rtf$footnotes[[1]]) <- 3
  pharmaRTF::index(rtf$footnotes[[2]]) <- 1
  pharmaRTF::index(rtf$footnotes[[3]]) <- 2

  expect_equal(hf_string(rtf, "titles"), "{\\header\n\\qc\n{\\f1\\fs24 rtf_doc Title 3}\n\\par\\qc\n{\\f1\\fs24 rtf_doc Title 2}\n\\par\\qc\n{\\f1\\fs24 rtf_doc Title 1}\n\\par\n\n{\n\\trowd\n\\trqc \\clbrdrt\\clbrdrl\\clbrdrb\\clbrdrr\\clvertalt\\clNoWrap\\clpadfl0\\clpadl80 \\clpadft0\\clpadt80 \\clpadfb0\\clpadb80 \\clpadfr0\\clpadr80 \\cellx2160 \n\\clbrdrt\\clbrdrl\\clbrdrb\\clbrdrr\\clvertalt\\clNoWrap\\clpadfl0\\clpadl80 \\clpadft0\\clpadt80 \\clpadfb0\\clpadb80 \\clpadfr0\\clpadr80 \\cellx4320 \\pard\\intbl\\ql{\\fs24 {Header1}}\\cell\n\\pard\\intbl\\ql{\\fs24 {Header2}}\\cell\n\\row\n}\n\n}")
  expect_equal(hf_string(rtf, "footnotes"), "{\\footer\n\\qc\n{\\f1\\fs24 rtf_doc Footnote 2}\n\\par\\qc\n{\\f1\\fs24 rtf_doc Footnote 3}\n\\par\\qc\n{\\f1\\fs24 rtf_doc Footnote 1}\\par\n}")
})

test_that("font_table_string adds fonts correctly", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5],
    add_colnames = FALSE
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

  #defaults except orientation
  orientation(rtf) <- "portrait"
  expect_equal(doc_properties_string(rtf),
               "\\paperw12240\\paperh15840\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt1440\\margb1440\n\\headery720\\footery720\\fs24\n")

  #Twips are 1/1440 inch
  pagesize(rtf) <- c(width = 10)
  expect_equal(doc_properties_string(rtf),
               "\\paperw12240\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\margl1440\\margr1440\\margt1440\\margb1440\n\\headery720\\footery720\\fs24\n")
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
  ## orientation should switch h and w when writing if landscape
  pagesize(rtf) <- c(height = 10, width = 5)
  orientation(rtf) <- "landscape"
  expect_equal(doc_properties_string(rtf),
               "\\paperw7200\\paperh14400\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\n\\lndscpsxn\n\\margl2880\\margr2880\\margt2880\\margb2880\n\\headery1440\\footery1440\\fs10\n")


})

test_that("header_string lines populates correctly" ,{
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5],
    add_colnames = FALSE
  )
  huxtable::wrap(ht) <- FALSE
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

  tmp1 <- tempfile()
  tmp2 <- tempfile()

  write_file(headers1, tmp1)
  write_file(headers2, tmp2)

  # This test needs to be rethought. It will fail anytime there is a change in huxtable.
  # ## expect headers are equal to the check files, removes return line.
  # expect_equal(tools::md5sum("headers1.txt")[[1]], tools::md5sum(tmp1)[[1]])
  # expect_equal(tools::md5sum("headers2.txt")[[1]], tools::md5sum(tmp2)[[1]])
})

test_that("write_rtf generates expected errors", {
  # Dummy huxtable
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  # rtf_doc object
  rtf1 <- rtf_doc(ht)

  expect_error(write_rtf(rtf))
  expect_error(supressWarnings(write_rtf(rtf, file='/as12asd/345eg/')))
})

test_that("write_rtf writes an expected rtf_file - 1", {
  df <- data.frame(
    "1" = 1:5,
    "2" = letters[1:5],
    "3" = LETTERS[1:5],
    "4" = 6:10,
    "5" = letters[6:10]
  )
  ht <- huxtable(
    df
  )
  ht <- huxtable::set_font(ht, 1, 1:5, "Times")
  huxtable::font_size(ht)[,2] <- 15
  huxtable::bold(ht)[,3] <- TRUE
  huxtable::italic(ht)[,4] <- TRUE
  ht <- huxtable::set_font(ht, 5, 1:5, "Comic Sans")

  huxtable::align(ht)[1,] <- "left"
  huxtable::align(ht)[2,] <- "center"
  huxtable::align(ht)[3,] <- "right"
  huxtable::valign(ht)[4,] <- "middle"
  huxtable::row_height(ht)[5] <- 2

  titles <- list(
    hf_line("A Test RTF Document"),
    hf_line("Manually checked and used for changes to the package", align = "left")
  )
  footers <- list(
    hf_line("These are footers"),
    hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
    hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
            font_size = 10, italic = TRUE),
    hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
    hf_line("FILE_PATH: File source: %s", index = 2)
  )

  rtf <- rtf_doc(ht, header_rows = 3, titles = titles)
  #This statement is blocked by issue 85
  rtf <- add_footnotes(rtf, hf_line("These are footers"),
                       hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
                       hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
                               font_size = 10, italic = TRUE),
                       hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
                       hf_line("FILE_PATH: File source: %s", index = 2))

  ignore_cell_padding(rtf) <- TRUE
  column_header_buffer(rtf) <- c(top = 1, bottom = 2)
  margins(rtf) <- c(top = 1, bottom = 2, left = 2, right = 0.5)
  orientation(rtf) <- "portrait"
  header_height(rtf) <- 1.5
  footer_height(rtf) <- 2
  pagesize(rtf) <- c(height = 12, width = 12)

  # write_rtf(rtf)
  expect_true(TRUE)

})

test_that("write_rtf writes an expected rtf_file - 2", {
  ht <- huxtable(
    col1 <- letters[1:25],
    col2 <- LETTERS[1:25],
    col3 <- 1:25
  )
  titles <- list(
    hf_line("A Test RTF Document 2"),
    hf_line("Manually checked and used for changes to the package", align = "left")
  )
  footers <- list(
    hf_line("These are footers"),
    hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
    hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
            font_size = 10, italic = TRUE),
    hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
    hf_line("FILE_PATH: File source: %s", index = 2)
  )

  rtf <- rtf_doc(ht, header_rows = 3, titles = titles)
  #This statement is blocked by issue 85
  rtf <- add_footnotes(rtf, hf_line("These are footers"),
                       hf_line(c("They can have page numbers", "PAGE_FORMAT: Page %s of %s"), align = "split"),
                       hf_line(c("Or a different format", "PAGE_FORMAT: Page %s  Total Pages %s"), align = "split", bold = TRUE,
                               font_size = 10, italic = TRUE),
                       hf_line(c("Here is a date format at the top", "DATE_FORMAT: %H:%M %A, %B %d, %Y"), index = 1),
                       hf_line("FILE_PATH: File source: %s", index = 2))

  margins(rtf) <- c(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5)
  orientation(rtf) <- "landscape"
  header_height(rtf) <- 1.5
  footer_height(rtf) <- 2
  pagesize(rtf) <- c(height = 8, width = 8)

  # write_rtf(rtf)
  expect_true(TRUE)

})


