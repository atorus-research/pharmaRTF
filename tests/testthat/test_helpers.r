context("helpers")
library(huxtable)
library(readr)
library(stringr)

test_that("insert_buffer adds a blank column correctly", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  ht_head1 <- huxtable(
    column1 = as.integer(1),
    column2 = "a"
  )
  ht_head2 <- huxtable(
    column1 = c("", "1"),
    column2 = c("", "a")
  )
  ht_head3 <- huxtable(
    column1 = c("", "1", "", ""),
    column2 = c("", "a", "", "")
  )
  ht_head4 <- huxtable(
    column1 = as.integer(1),
    column2 = c("a")
  )

  ## adding this attribute back in because this is stored in the doc os it
  # isn't needed here.
  attr(ht_head1, "header_rows") <- 1
  attr(ht_head2, "header_rows") <- 1
  attr(ht_head3, "header_rows") <- 1
  attr(ht_head4, "header_rows") <- 1

  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head1))
  column_header_buffer(rtf) <- list(top = 1)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head2))
  column_header_buffer(rtf) <- list(bottom = 2)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head3))
  column_header_buffer(rtf) <- list(top = 0, bottom = 0)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head4))

  ### Testing when header_rows == 2
  rtf2 <- rtf_doc(ht, header_rows = 2)
  ht2_head1 <- huxtable(
    column1 = as.integer(c(1, 2)),
    column2 = c("a", "b")
  )
  ht2_head2 <- huxtable(
    column1 = c("", "1", "2"),
    column2 = c("", "a", "b")
  )
  ht2_head3 <- huxtable(
    column1 = c("", "1", "2", "", ""),
    column2 = c("", "a", "b", "", "")
  )
  ht2_head4 <- huxtable(
    column1 = as.integer(c(1, 2)),
    column2 = c("a", "b")
  )

  expect_true(dplyr::all_equal(insert_buffer(rtf2, rtf2$table[1:header_rows(rtf2$table)]), ht2_head1))
  column_header_buffer(rtf2) <- list(top = 1)
  expect_true(dplyr::all_equal(insert_buffer(rtf2, rtf2$table[1:header_rows(rtf2$table)]), ht2_head2))
  column_header_buffer(rtf2) <- list(bottom = 2)
  expect_true(dplyr::all_equal(insert_buffer(rtf2, rtf2$table[1:header_rows(rtf2$table)]), ht2_head3))
  column_header_buffer(rtf2) <- list(top = 0, bottom = 0)
  expect_true(dplyr::all_equal(insert_buffer(rtf2, rtf2$table[1:header_rows(rtf2$table)]), ht2_head4))

})

test_that("needs_buffer returns the correct value", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  expect_false(needs_buffer(rtf))

  column_header_buffer(rtf) <- c(top=1)
  expect_true(needs_buffer(rtf))

  column_header_buffer(rtf) <- c(top=0)
  expect_false(needs_buffer(rtf))
})

test_that("reaplce_cell_padding replaces padding flags correctly", {
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  write_rtf(rtf)

  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfl3"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadft3"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfb3"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfr3"))

  ignore_cell_padding(rtf) <- TRUE
  write_rtf(rtf)
  expect_false(str_detect(read_file("test.rtf"), "\\\\clpadfl3"))
  expect_false(str_detect(read_file("test.rtf"), "\\\\clpadft3"))
  expect_false(str_detect(read_file("test.rtf"), "\\\\clpadfb3"))
  expect_false(str_detect(read_file("test.rtf"), "\\\\clpadfr3"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfl0"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadft0"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfb0"))
  expect_true(str_detect(read_file("test.rtf"), "\\\\clpadfr0"))
})

test_that("format_text_string placeholder test", {
  pgFormat1 <- "PAGE_FORMAT: abc123 %s and %s"
  pgFormat2 <- "PAGE_FORMAT: abc123:: %s and %s"
  expect_equal(format_text_string(pgFormat1),
              "{abc123 }{\\field\\flddirty{\\*\\fldinst{  PAGE   \\\\* MERGEFORMAT }}}{ and }{\\field{\\*\\fldinst{ NUMPAGES}}}")
  expect_equal(format_text_string(pgFormat2),
               "{abc123:: }{\\field\\flddirty{\\*\\fldinst{  PAGE   \\\\* MERGEFORMAT }}}{ and }{\\field{\\*\\fldinst{ NUMPAGES}}}")

  dtFormat1 <- "DATE_FORMAT: %Y-%m-%d"
  expect_equal(format_text_string(dtFormat1), paste0("{", Sys.Date(), "}"))

})


test_that("correct_defaults retuns the correct defaults", {

  expect_equal(correct_defaults("type"), "")
  expect_equal(correct_defaults("text1"), "")
  expect_equal(correct_defaults("text2"), "")
  expect_equal(correct_defaults("align"), "center")
  expect_equal(correct_defaults("font"), as.character(NA))
  expect_equal(correct_defaults("bold"), FALSE)
  expect_equal(correct_defaults("italic"), FALSE)
  expect_equal(correct_defaults("index"), NA)
  expect_equal(correct_defaults("font_size"), as.numeric(NA))


})



