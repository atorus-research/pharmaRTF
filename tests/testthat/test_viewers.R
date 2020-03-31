context("viewers")
library("huxtable")



test_that("Viewer functions throw error when no headers/footers to display", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_error(view_hf(rtf), "attempt to select less than one element in get1index")
})
