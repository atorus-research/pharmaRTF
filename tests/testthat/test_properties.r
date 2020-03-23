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

  rtf <- as_rtf_doc(ht, aTitle, aFooter)

  #Is currently missing
  expect_setequal(font(rtf), allFonts)
})







#### Errors ####
test_that("font throws error when given a non-character", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)

  expect_error(font(rtf) <- 1, "value is not a character vector")
})

test_that("font_size throws error when given a non-numeric", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)

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
  expect_error(align(x2) <- "left")
  expect_error(align(x2) <- "right")
  expect_error(align(x2) <- "center")
})

test_that("bold throws error when given a non-logical", {
  x <- hf_line()

  expect_error(bold(x) <- "abc", "is.logical")
})

test_that("bold throws error when given a non-logical", {
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

  expect_error(index(x) <- "asdf", "is.numeric(value) | is.null(value) is not TRUE")
  expect_silent(index(x) <- NULL)
  expect_silent(index(x) <- 1)
})

test_that("margins throws error when given a bad parameter", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)
  rtf_mar <- list(
    top = 1,
    bottom = 2,
    corner = 3
  )

  expect_error(margins(rtf) <- rtf_mar, "Invalid parameter")
})

test_that("orientation throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)

  expect_error(orientation(rtf) <- "flipped", "'arg' should be one of")
})

test_that("pagesize throws error when bad parameter is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)
  rtf_ps <- list(
    height = 5,
    middle = 1
  )

  expect_error(pagesize(rtf) <- rtf_ps, "Invalid parameters")
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

  expect_error(header_rows(ht) <- 1.5, "Header rows must be a whole number")
  expect_error(header_rows(ht) <- "asdf", "Header rows must be a whole number")
})

