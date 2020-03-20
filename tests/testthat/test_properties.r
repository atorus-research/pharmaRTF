## TODO:
# Add tests for gt set and get
context("properties")
library(huxtable)

# library(gt)

ht_tab <- hux(
  column1 = 1:5,
  column2 = letters[1:5]
)

huxtable::font(ht_tab)[1,1] <- "Times New Roman"
huxtable::font(ht_tab)[2,2] <- "Comic Sans"
huxtable::font(ht_tab)[4,2] <- "Windings"


test_that("Font getters property return all font(s)", {



  expect_true(all(font(ht_tab) %in% c("Times New Roman", NA, "Comic Sans", "Windings")) &
              all(c("Times New Roman", NA, "Comic Sans", "Windings") %in% font(ht_tab)) &
              length(font(ht_tab) == 4))
})

