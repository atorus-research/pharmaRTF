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

test_that('T2.01',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify margins are set to expected default:1,1,1,1
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(rtf))

  # Change margins to 2,.5,1.5,.25 and output
  pharmaRTF::margins(rtf) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
  pharmaRTF::write_rtf(rtf, file='test_2_01.rtf')

  rm(rtf)
})

test_that('T2.02',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify header and footer heights are set to expected default:.5,.5
  testthat::expect_equal(.5, pharmaRTF::header_height(rtf)) %>%
    testthat::expect_equal(.5, pharmaRTF::footer_height(rtf))

  # Change header and footer heights to .25,1 and output
  pharmaRTF::header_height(rtf) <- .25
  pharmaRTF::footer_height(rtf) <- 1
  pharmaRTF::write_rtf(rtf, file='test_2_02.rtf')

  rm(rtf)
})

test_that('T2.03',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify page size is set to expected default:8.5,11
  testthat::expect_equal(c(height = 8.5, width = 11), pharmaRTF::pagesize(rtf))

  # Change page size to 9,12 and output
  pharmaRTF::pagesize(rtf) <- c(height = 9, width = 12)
  pharmaRTF::write_rtf(rtf, file='test_2_03.rtf')

  rm(rtf)
})

test_that('T2.04',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify orientation is set to expected default:landscape
  testthat::expect_equal("landscape",pharmaRTF::orientation(rtf))

  # Change orientation to portrait and output
  pharmaRTF::orientation(rtf) <- "portrait"
  pharmaRTF::write_rtf(rtf, file='test_2_04.rtf')

  rm(rtf)
})

test_that('T2.05',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify font size is set to expected default:12
  testthat::expect_equal(12,pharmaRTF::font_size(rtf))

  # Change font size to 14 and output
  pharmaRTF::font_size(rtf) <- 14
  pharmaRTF::write_rtf(rtf, file='test_2_05.rtf')

  rm(rtf)
})

test_that('T2.06',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify font is set to expected default:Courier New
  testthat::expect_equal("Courier New",pharmaRTF::font(rtf))

  # Change font to Comic Sans and output
  pharmaRTF::font(rtf) <- "Comic Sans"
  pharmaRTF::write_rtf(rtf, file='test_2_06.rtf')

  rm(rtf)
})

test_that('T2.07',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify header rows is set to expected default:1
  testthat::expect_equal(1,pharmaRTF::header_rows(rtf))

  # Change header rows to 2 and output
  pharmaRTF::header_rows(rtf) <- 2
  pharmaRTF::write_rtf(rtf, file='test_2_07.rtf')

  rm(rtf)
})

test_that('T2.08',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify column header buffers are set to expected default:0,0
  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(rtf))

  # Change column header buffers to 2,1 and output
  pharmaRTF::column_header_buffer(rtf) <- c(top = 2, bottom = 1)
  pharmaRTF::write_rtf(rtf, file='test_2_08.rtf')

  rm(rtf)
})

test_that('T2.09',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify ignore cell padding is set to expected default:FALSE
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(rtf))

  # Change ignore cell padding to TRUE and output
  pharmaRTF::ignore_cell_padding(rtf) <- TRUE
  pharmaRTF::write_rtf(rtf, file='test_2_09.rtf')

  rm(rtf)
})
