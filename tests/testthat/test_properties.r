## TODO:
# Add tests for gt set and get
context("properties")
library(huxtable)
# library(gt)


test_that("font returns the correct vector for an rtf object", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  huxtable::font(ht) <- "TableFont"

  aTitle <- hf_line("theTitle", font = "FontTitle")
  aFooter <- hf_line("theFooter", font = "FontFooter")
  allFonts <- c("Courier New", "TableFont", "FontTitle", "FontFooter")

  rtf <- rtf_doc(ht, list(aTitle), list(aFooter))

  expect_setequal(font(rtf), allFonts)
})

test_that("text returns the correct vector for a hf_line", {
  x1 <- hf_line("text")
  x2 <- hf_line(c("text1", "text2"))

  expect_equal(text(x1), c("text", ""))
  expect_equal(text(x2), c("text1", "text2"))
})


#### Errors ####
test_that("font throws error when given a non-character", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(font(rtf) <- 1, "value is not a character vector")
})

test_that("font_size throws error when given a non-numeric", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(font_size(ht) <- "abc")
})

test_that("align throws error when given a bad value" , {
  x <- hf_line()

  expect_error(align(x) <- "Flipped", "'arg' should be one of")

})

test_that("align throws error when given incorrect number of arguments", {
  x1 <- hf_line(c("text1"))
  x2 <- hf_line(c("text1", "text2"))

  expect_error(align(x1) <- "split", "There must be two")
})

test_that("bold throws error when given a non-logical", {
  x <- hf_line()

  expect_error(bold(x) <- "abc", "is.logical")
})

test_that("italic throws error when given a non-logical", {
  x <- hf_line()

  expect_error(italic(x) <- "abc", "is.logical")
})

test_that("text throws error when given a non-character, bad number of text lengths", {
  x <- hf_line()


  expect_error(text(x) <- 0)
  expect_error(text(x) <- c("oneText", "twoText", "threeText"))
})

test_that("index throws error when given a bad parameter and will accept null values", {
  x <- hf_line()

  expect_error(index(x) <- "asdf", "is not TRUE")
  expect_silent(index(x) <- NULL)
  expect_silent(index(x) <- 1)
})

test_that("margins throws error when given a bad parameter", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)
  rtf_mar1 <- list(
    top = 1,
    bottom = 2,
    corner = 3
  )
  rtf_mar2 <- list(
    top = 1,
    bottom = "2",
    left = 3,
    right = "abc"
  )
  rtf_mar3 <- list(
    top = 4,
    top = 2
  )
  rtf_mar4 <- list(
    top = -2
  )

  expect_error(margins(rtf) <- rtf_mar1, "Invalid parameter")
  expect_error(margins(rtf) <- rtf_mar2, "Margins must be positive numbers")
  expect_error(margins(rtf) <- rtf_mar3, "Duplicate parameters entered")
  expect_error(margins(rtf) <- rtf_mar4, "Margins must be positive numbers")
})

test_that("orientation throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(orientation(rtf) <- "flipped", "'arg' should be one of")
})

test_that("header_height throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(header_height(rtf) <- "1234", "value is not a numeric or integer vector")
})

test_that("footer_height throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(footer_height(rtf) <- "1234", "value is not a numeric or integer vector")
})

test_that("pagesize throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)
  rtf_ps1 <- list(
    height = 5,
    middle = 1
  )
  rtf_ps2 <- list(
    height = 5,
    width = "asdf"
  )
  rtf_ps3 <- list(
    height = "1",
    width = "3"
  )
  rtf_ps4 <- list(
    height = 1,
    height = 4
  )
  rtf_ps5 <- list(
    height = -1
  )

  expect_error(pagesize(rtf) <- rtf_ps1, "Invalid parameters")
  expect_error(pagesize(rtf) <- rtf_ps2, "Height and Width must be positive numbers")
  expect_error(pagesize(rtf) <- rtf_ps3, "Height and Width must be positive numbers")
  expect_error(pagesize(rtf) <- rtf_ps4, "Duplicate parameters entered")
  expect_error(pagesize(rtf) <- rtf_ps5, "Height and Width must be positive numbers")
})

test_that("header_rows throws error when passed a gt table", {
  x <- list()
  class(x) <- "gt_tbl"

  expect_error(header_rows(x) <- "asdf", "GT tables do not require header rows to be set")
  expect_error(header_rows(x), "GT tables do not require header rows to be set")
})

test_that("header rows throws error when passed a bad parameter", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )

  expect_error(header_rows(ht) <- 1.5, "Header rows must be a positive whole number")
  expect_error(header_rows(ht) <- "asdf", "Header rows must be a positive whole number")
  expect_error(header_rows(ht) <- -1, "pos")
})

test_that("ignore_cell_padding throws error when passed a bad parameter", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(ignore_cell_padding(rtf) <- "abc", "is not TRUE")
})

test_that("column_header_buffer<-/set_column_header buffer throw errors as expected", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  val1 <- list(
    top = 3,
    below = 2
  )
  val2 <- list(
    top = 1,
    top = 3,
    bottom = 1
  )
  val3 <- list(
    top = "1",
    bottom = "2"
  )
  val4 <- list(
    top = -1
  )

  expect_error(set_column_header_buffer(rtf, "2", 1), "Top and bottom values must be positive whole numbers")
  expect_error(set_column_header_buffer(rtf, 2, 1.4), "Top and bottom values must be positive whole numbers")
  expect_error(set_column_header_buffer(rtf, c(1,2), 2), "not implemented")
  expect_error(column_header_buffer(rtf) <- val1, "Invalid named element")
  expect_error(column_header_buffer(rtf) <- val2, "Duplicate parameters entered")
  expect_error(column_header_buffer(rtf) <- val3, "whole numbers")
  expect_error(column_header_buffer(rtf) <- val4, "Top and bottom values must be positive whole numbers")
})



