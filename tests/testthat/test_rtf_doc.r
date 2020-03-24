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
