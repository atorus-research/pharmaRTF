context("rtf_doc")

# Packages
library(huxtable)
library(pharmaRTF)

#### Errors ####
test_that("rtf_doc throws error when passed unsupported class", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  class(ht) <- "anUnsupportedClass"
  expect_error(as_rtf_doc(ht), "Unsupported table type")
})
