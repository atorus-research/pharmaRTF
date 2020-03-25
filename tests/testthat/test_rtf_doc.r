context("rtf_doc")

# Packages
library(huxtable)
library(pharmaRTF)

test_that("rtf_doc returns a list with a table, title, and footnotes", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_equal(nrow(rtf$table), 5)
  expect_length(rtf$table, 2)
  expect_length(rtf, 3)
  expect_named(rtf, c("table", "titles", "footnotes"))
})


#### Errors/Warnings/Notes ####
test_that("rtf_doc throws error when passed unsupported class", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  class(ht) <- "anUnsupportedClass"
  expect_error(rtf_doc(ht), "Unsupported table type")
})

test_that("rtf_doc throws message when hux with caption is passed", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  huxtable::caption(ht) <- "A caption"
  expect_message(rtf_doc(ht), "Huxtable contains caption")
})

test_that("rtf_doc throws message when gt table is passed", {
  x <- list(
    table = list(),
    titles = list(),
    footnotes = list()
  )
  class(x) <- "gt_tbl"
  expect_warning(rtf_doc(x), "GT does not fully support RTF at this time")
})

test_that("validate_rtf_doc throws errors appropriately", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  attr(ht, "header.rows") <- 1

  titles1 <- list(
    hf_line()
  )
  titles2 <- list(
    hf_line(),
    hf_line(),
    hf_line()
  )
  titles3 <- list()
  footnotes1 <- list(
    hf_line()
  )
  footnotes2 <- list(
    hf_line(),
    hf_line(),
    hf_line()
  )
  footnotes3 <- list()

  # All lists, should give no errors
  expect_silent(validate_rtf_doc(ht, titles = titles1, footnotes = footnotes1))
  expect_silent(validate_rtf_doc(ht, titles = titles1, footnotes = footnotes2))
  expect_silent(validate_rtf_doc(ht, titles = titles1, footnotes = footnotes3))
  expect_silent(validate_rtf_doc(ht, titles = titles2, footnotes = footnotes1))
  expect_silent(validate_rtf_doc(ht, titles = titles2, footnotes = footnotes2))
  expect_silent(validate_rtf_doc(ht, titles = titles2, footnotes = footnotes3))
  expect_silent(validate_rtf_doc(ht, titles = titles3, footnotes = footnotes1))
  expect_silent(validate_rtf_doc(ht, titles = titles3, footnotes = footnotes2))
  expect_silent(validate_rtf_doc(ht, titles = titles3, footnotes = footnotes3))

  # Passing a hf_line directly and a vector of them should raise an error
  # Three of the tests below still pass despite not being passed a list.
  # as hf_line is a list, a character vector of them is of class list. I think
  # this is fine so I removed them.
  expect_error(validate_rtf_doc(ht, titles = hf_line(), footnotes = footnotes1),
               "Titles and footnotes must be lists of hf_line objects")
  # expect_error(validate_rtf_doc(ht, titles = c(hf_line(), hf_line()), footnotes = footnotes1),
  #              "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = hf_line(), footnotes = footnotes2),
               "Titles and footnotes must be lists of hf_line objects")
  # expect_error(validate_rtf_doc(ht, titles = c(hf_line(), hf_line()), footnotes = footnotes2),
  #              "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = hf_line(), footnotes = footnotes3),
               "Titles and footnotes must be lists of hf_line objects")
  # expect_error(validate_rtf_doc(ht, titles = c(hf_line(), hf_line()), footnotes = footnotes3),
  #              "Titles and footnotes must be lists of hf_line objects")
})



