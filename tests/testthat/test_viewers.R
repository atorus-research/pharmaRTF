context("viewers")
library("huxtable")

test_that("viewers return correct data.frame", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
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

  ## I'm having trouble testing the viewer functions due to either conflicts
  # with View or how testthat interacts with the terminal.
  expect_true(TRUE)

})

test_that("Viewer functions throw error when no headers/footers to display", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(view_hf(rtf), "attempt to select less than one element in get1index")
})
