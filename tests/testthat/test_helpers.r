context("helpers")
library(huxtable)

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
  attr(ht_head1, "header.rows") <- 1
  attr(ht_head2, "header.rows") <- 1
  attr(ht_head3, "header.rows") <- 1
  attr(ht_head4, "header.rows") <- 1

  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head1))
  column_header_buffer(rtf) <- list(top = 1)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head2))
  column_header_buffer(rtf) <- list(bottom = 2)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head3))
  column_header_buffer(rtf) <- list(top = 0, bottom = 0)
  expect_true(dplyr::all_equal(insert_buffer(rtf, rtf$table[1:header_rows(rtf$table)]), ht_head4))

  ### Testing when header.rows == 2
  rtf2 <- rtf_doc(ht, header.rows = 2)
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

test_that("format_test_string placeholder test", {
  expect_true(TRUE)
})
