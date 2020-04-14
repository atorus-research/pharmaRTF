context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)
library(testthat)

ht <- huxtable::huxtable(
  column1 = c("Header1", 1:10),
  column2 = c("Header2", letters[1:10])
)

test_that('T 2.1.1',{

  rtf1 <- pharmaRTF::rtf_doc(ht)

  # Verify margins are set to expected default:1,1,1,1
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(rtf1))

  # Change margins to 2,.5,1.5,.25 and check output to verify they have been changed
  pharmaRTF::margins(rtf1) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
  pharmaRTF::write_rtf(rtf1, file='test_2_1_1.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.2',{

  rtf2 <- pharmaRTF::rtf_doc(ht)

  # Verify header and footer heights are set to expected default:.5,.5
  testthat::expect_equal(.5, pharmaRTF::header_height(rtf2)) %>%
    testthat::expect_equal(.5, pharmaRTF::footer_height(rtf2))

  # Change header and footer heights to .25,1 and check output to verify they have been changed
  pharmaRTF::header_height(rtf2) <- .25
  pharmaRTF::footer_height(rtf2) <- 1
  pharmaRTF::write_rtf(rtf2, file='test_2_1_2.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.3',{

  rtf3 <- pharmaRTF::rtf_doc(ht)

  # Verify page size is set to expected default:8.5,11
  testthat::expect_equal(c(height = 8.5, width = 11), pharmaRTF::pagesize(rtf3))

  # Change page size to 9,12 and check output to confirm it has been changed
  pharmaRTF::pagesize(rtf3) <- c(height = 9, width = 12)
  pharmaRTF::write_rtf(rtf3, file='test_2_1_3.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.4',{

  rtf4 <- pharmaRTF::rtf_doc(ht)

  # Verify orientation is set to expected default:landscape
  testthat::expect_equal("landscape",pharmaRTF::orientation(rtf4))

  # Change orientation to portrait and check output to confirm it has been changed
  pharmaRTF::orientation(rtf4) <- "portrait"
  pharmaRTF::write_rtf(rtf4, file='test_2_1_4.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.5',{

  rtf5 <- pharmaRTF::rtf_doc(ht)

  # Verify font size is set to expected default:12
  testthat::expect_equal(12,pharmaRTF::font_size(rtf5))

  # Change font size to 14 and check output to confirm it has been changed
  pharmaRTF::font_size(rtf5) <- 14
  pharmaRTF::write_rtf(rtf5, file='test_2_1_5.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.6',{

  rtf6 <- pharmaRTF::rtf_doc(ht)

  # Verify font is set to expected default:Courier New
  testthat::expect_equal("Courier New",pharmaRTF::font(rtf6))

  # Change font to Comic Sans and check output to confirm it has been changed
  pharmaRTF::font(rtf6) <- "Comic Sans"
  pharmaRTF::write_rtf(rtf6, file='test_2_1_6.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.7',{

  rtf7 <- pharmaRTF::rtf_doc(ht)

  # Verify header rows is set to expected default:1
  testthat::expect_equal(1,pharmaRTF::header_rows(rtf7))

  # Change header rows to 2 and check output to confirm it has been changed
  pharmaRTF::header_rows(rtf7) <- 2
  pharmaRTF::write_rtf(rtf7, file='test_2_1_7.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.8',{

  rtf8 <- pharmaRTF::rtf_doc(ht)

  # Verify column header buffers are set to expected default:0,0
  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(rtf8))

  # Change column header buffers to 2,1 and check output to verify they have been changed
  pharmaRTF::column_header_buffer(rtf8) <- c(top = 2, bottom = 1)
  pharmaRTF::write_rtf(rtf8, file='test_2_1_8.rtf')

  #expect_true(FALSE)
})

test_that('T 2.1.9',{

  rtf9 <- pharmaRTF::rtf_doc(ht)

  # Verify ignore cell padding is set to expected default:FALSE
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(rtf9))

  # Change ignore cell padding to TRUE and check output to confirm it has been changed
  pharmaRTF::ignore_cell_padding(rtf9) <- TRUE
  pharmaRTF::write_rtf(rtf9, file='test_2_1_9.rtf')

  #expect_true(FALSE)
})
