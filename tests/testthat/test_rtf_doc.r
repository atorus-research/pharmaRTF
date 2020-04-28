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

test_that("rtf_doc returns the correct ordering of titles", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  titles_l <- list(
    hf_line("test3"),
    hf_line("test2", index=2),
    hf_line("test1", index=1),
    hf_line("test4")
  )
  footers_l <- list(
    hf_line("ftest3"),
    hf_line("ftest2", index=2),
    hf_line("ftest1", index=1),
    hf_line("ftest4")
  )

  rtf <- rtf_doc(ht, titles = titles_l, footnotes = footers_l)

  expect_equal(unname(unlist(rtf$titles)), c("test1", "test2", "test3", "test4"))
  expect_equal(unname(unlist(rtf$footnotes)), c("ftest1", "ftest2", "ftest3", "ftest4"))
})

test_that("rtf_doc returns a logical(0) when passed no table", {
  expect_true(is.logical(rtf_doc()))
  expect_length(rtf_doc(), 0)
  expect_is(rtf_doc(), "rtf_doc")
})

test_that("rtf_doc generates and returns the expected items", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(rtf_doc(rtf), "An `rtf_doc` object was provided")
})

test_that("rtf_doc sets the correct defaults", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_equal(font(rtf) , "Courier New")
  expect_equal(font_size(rtf), 12)
  expect_equal(margins(rtf), c(top = 1, bottom = 1, left = 1, right = 1))
  expect_equal(orientation(rtf), "landscape")
  expect_equal(header_height(rtf), 0.5)
  expect_equal(footer_height(rtf), 0.5)
  expect_equal(pagesize(rtf), c(width = 8.5, height = 11))
  expect_equal(header_rows(rtf), 1)
  expect_equal(ignore_cell_padding(rtf), FALSE)
  expect_equal(column_header_buffer(rtf), c(top = 0, bottom = 0))

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
  attr(ht, "header_rows") <- 1

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
  expect_error(validate_rtf_doc(ht, titles = hf_line("test"), footnotes = footnotes1),
               "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = c(hf_line("test"), hf_line()), footnotes = footnotes1),
               "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = hf_line("test"), footnotes = footnotes2),
               "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = c(hf_line("test"), hf_line()), footnotes = footnotes2),
               "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = hf_line("test"), footnotes = footnotes3),
               "Titles and footnotes must be lists of hf_line objects")
  expect_error(validate_rtf_doc(ht, titles = c(hf_line("test"), hf_line()), footnotes = footnotes3),
               "Titles and footnotes must be lists of hf_line objects")
})



