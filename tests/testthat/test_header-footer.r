context("header-footer.R")
library(huxtable)


### Written to be consistant with S3 class concepts



test_that("placeholder", {
  expect_true(TRUE)
})



#### Errors ####
test_that("add_hf throws error when given something thats not a hf_line", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- as_rtf_doc(ht)
  x <- list()
  x_l <- list("a", "b")

  expect_error(add_hf(rtf, x, to = "titles"))
  expect_error(add_hf(rtf, x_l, to = "titles"))
})
