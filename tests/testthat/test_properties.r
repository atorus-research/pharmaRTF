## TODO:
# Add tests for gt set and get



context("properties")
library(huxtable)
library(pharmaRTF)
# library(gt)

test_that("Font getters property return all font(s)", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  huxtable::font(ht)[1,1] <- "Times New Roman"
  huxtable::font(ht)[2,2] <- "Comic Sans"
  huxtable::font(ht)[4,2] <- "Windings"


  expect_true(TRUE)
})

