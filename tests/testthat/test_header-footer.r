context("header-footer.R")
library(huxtable)


test_that("hf_line returns a hf_line object", {
  x1 <- hf_line()
  x2 <- hf_line(c("abc"))
  x3 <- hf_line(c("abc", "xyz"))


  expect_s3_class(x1, "hf_line")
  expect_is(x2$text, "character")
  expect_is(x3$text, "character")
})

test_that("hf_line ignores NAs", {
  expect_silent(hf_line(c(NA, "abc", NA, "xyz")))

  x <- hf_line(c(NA, "abc", NA, "xyz"))
  expect_equal(text(x), c("abc", "xyz"))
})

test_that("order_lines properly orders lines with nulls at the back", {
  x_l <- list(
    hf_line(index = NULL),
    hf_line(index = 2),
    hf_line(index = 3),
    hf_line(index = NULL),
    hf_line(index = 4),
    hf_line(index = 1),
    hf_line(index = NULL)
  )

  expect_equal(lapply(order_lines(x_l), attr, which = "index"), list(1, 2, 3, 4, NULL, NULL, NULL))
})

test_that("add_hf replaces lines when appropriate", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)
  rtf <- add_hf(rtf, hf_line("abc"), to = "titles")

  expect_equivalent(rtf$titles, list("abc"))

  rtf <- add_hf(rtf, hf_line("xyz"), to = "titles", replace = FALSE)

  expect_equivalent(rtf$titles, list("abc", "xyz"))

  rtf <- add_hf(rtf, hf_line("123"), to = "titles", replace = TRUE)

  expect_equivalent(rtf$titles, list("123"))
})


#### Errors ####
test_that("hf_line throws error when given bad align", {
  expect_error(hf_line("asdf", align = "middle"))
})

test_that("add_hf throws error when given something thats not a hf_line", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)
  x <- list()
  x_l <- list("a", "b")

  expect_error(add_hf(rtf, x, to = "titles"))
  expect_error(add_hf(rtf, x_l, to = "titles"))
})

test_that("validate_hf_line throws errors appropriately", {
  x <- hf_line("abc")
  line <- "abc"
  align <- "center"
  bold <- FALSE
  italic <- FALSE
  font <- NA
  font_size <- NaN
  index <- NULL

  expect_silent(validate_hf_line(x, align(x), bold(x), italic(x), font(x), font_size(x), index(x)))
  #Test all validation tests
  expect_error(validate_hf_line(line = list(text = c("abc", "xzy", "qwerty")), align(x), bold(x), italic(x), font(x), font_size(x)),
               "No more than two entries may be provided per line")
  expect_error(validate_hf_line(x, align = "split", bold(x), italic(x), font(x), font_size(x), index(x)),
               "Two text entries must be provided if alignment is 'split'")
  expect_error(validate_hf_line(x, align(x), bold = "Yes", italic(x), font(x), font_size(x), index(x)),
               "is not TRUE")
  expect_error(validate_hf_line(x, align(x), bold(x), italic = "Yes", font(x), font_size(x), index(x)),
               "is not TRUE")
  expect_error(validate_hf_line(x, align(x), bold(x), italic(x), font = 1, font_size(x), index(x)),
               "is not TRUE")
  expect_error(validate_hf_line(x, align(x), bold(x), italic(x), font(x), font_size = "10", index(x)),
               "font_size is not a numeric or integer vector")
  ## Not nesscarily an error but my want to have more checks for index, positive integer
  expect_error(validate_hf_line(x, align(x), bold(x), italic(x), font(x), font_size(x), index = "1"),
               "is not TRUE")
  expect_error(validate_hf_line(x, align(x), bold(x), italic(x), font(x), font_size(x), index = 1.2),
               "is not TRUE")
  expect_error(validate_hf_line(x, align(x), bold(x), italic(x), font(x), font_size(x), index = -1),
               "is not TRUE")
})

test_that("order_lines throws error for duplicate indicies", {
  x_l <- list(
    hf_line(index = 1),
    hf_line(index = 1)
  )
  expect_error(order_lines(x_l), "Duplicate indices provided")
})

test_that("add_hf throws error when given non-hf_line objects", {
  x1 <- list()
  x2 <- list()
  class(x1) <- "hf_line"
  class(x2) <- "not_hf_line"

  expect_error(add_hf(NULL, list(x1,x2), replace = TRUE), "Provided titles must be hf_line objects")
})

