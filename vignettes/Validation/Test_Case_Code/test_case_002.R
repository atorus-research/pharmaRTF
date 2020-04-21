context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)
library(testthat)

vur <- NULL
if(file.exists("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")) vur <- readRDS("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")

test_that('T2.01',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_01 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_01, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_01.RData")

    # change margins to 2,.5,1.5,.25 and output rtf for manual review
    pharmaRTF::margins(test_2_01) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
    pharmaRTF::write_rtf(test_2_01, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_01.rtf')

    rm(ht)
    rm(test_2_01)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_01.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(test_2_01))
  testthat::expect_true(vur[vur$ID == "T2.01.02", "Response"])
})

test_that('T2.02',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_02 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_02, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_02.RData")

    # change header and footer heights to .25,1 and output rtf for manual review
    pharmaRTF::header_height(test_2_02) <- .25
    pharmaRTF::footer_height(test_2_02) <- 1
    pharmaRTF::write_rtf(test_2_02, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_02.rtf')

    rm(ht)
    rm(test_2_02)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_02.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(.5, pharmaRTF::header_height(test_2_02)) %>%
    testthat::expect_equal(.5, pharmaRTF::footer_height(test_2_02))
  testthat::expect_true(vur[vur$ID == "T2.02.02", "Response"])
})

test_that('T2.03',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_03 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_03, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_03.RData")

    # change page size to 9,12 and output rtf for manual review
    pharmaRTF::pagesize(test_2_03) <- c(height = 9, width = 12)
    pharmaRTF::write_rtf(test_2_03, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_03.rtf')

    rm(ht)
    rm(test_2_03)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_03.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(height = 8.5, width = 11), pharmaRTF::pagesize(test_2_03))
  testthat::expect_true(vur[vur$ID == "T2.03.02", "Response"])
})

test_that('T2.04',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_04 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_04, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_04.RData")

    # change orientation to portrait and output rtf for manual review
    pharmaRTF::orientation(test_2_04) <- "portrait"
    pharmaRTF::write_rtf(test_2_04, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_04.rtf')

    rm(ht)
    rm(test_2_04)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_04.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal("landscape",pharmaRTF::orientation(test_2_04))
  testthat::expect_true(vur[vur$ID == "T2.04.02", "Response"])
})

test_that('T2.05',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_05 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_05, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_05.RData")

    # change font size to 14 and output rtf for manual review
    pharmaRTF::font_size(test_2_05) <- 14
    pharmaRTF::write_rtf(test_2_05, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_05.rtf')

    rm(ht)
    rm(test_2_05)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_05.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(12,pharmaRTF::font_size(test_2_05))
  testthat::expect_true(vur[vur$ID == "T2.05.02", "Response"])
})

test_that('T2.06',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_06 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_06, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_06.RData")

    # change font to Comic Sans and output rtf for manual review
    pharmaRTF::font(test_2_06) <- "Comic Sans"
    pharmaRTF::write_rtf(test_2_06, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_06.rtf')

    rm(ht)
    rm(test_2_06)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_06.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal("Courier New",pharmaRTF::font(test_2_06))
  testthat::expect_true(vur[vur$ID == "T2.06.02", "Response"])
})

test_that('T2.07',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_07 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_07, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_07.RData")

    # change header rows to 2 and output rtf for manual review
    pharmaRTF::header_rows(test_2_07) <- 2
    pharmaRTF::write_rtf(test_2_07, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_07.rtf')

    rm(ht)
    rm(test_2_07)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_07.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(1,pharmaRTF::header_rows(test_2_07))
  testthat::expect_true(vur[vur$ID == "T2.07.02", "Response"])
})

test_that('T2.08',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_08 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_08, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_08.RData")

    # change column header buffers to 2,1 and output rtf for manual review
    pharmaRTF::column_header_buffer(test_2_08) <- c(top = 2, bottom = 1)
    pharmaRTF::write_rtf(test_2_08, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_08.rtf')

    rm(ht)
    rm(test_2_08)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_08.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(test_2_08))
  testthat::expect_true(vur[vur$ID == "T2.08.02", "Response"])
})

test_that('T2.09',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_2_09 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_2_09, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_09.RData")

    # change ignore cell padding to TRUE and output rtf for manual review
    pharmaRTF::ignore_cell_padding(test_2_09) <- TRUE
    pharmaRTF::write_rtf(test_2_09, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_09.rtf')

    rm(ht)
    rm(test_2_09)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_09.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(test_2_09))
  testthat::expect_true(vur[vur$ID == "T2.09.02", "Response"])
})

