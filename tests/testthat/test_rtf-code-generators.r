context("rtf-code-generators")
library(huxtable)

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
  rtf <- rtf_doc(ht)

  expect_true(TRUE)
})
