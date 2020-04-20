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

test_that('T1',{

  # output creation
  if(is.null(vur)) {
    # dataframe requiring one page of output
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )

    test_1 <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_1, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_1.rtf')

    rm(ht)
    rm(test_1)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T1C1", "Response"])
  expect_true(vur[vur$ID == "T1C2", "Response"])
})

test_that('T2',{

  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    test_2 <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2.rtf')

    rm(ht)
    rm(test_2)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T2C1", "Response"])
  expect_true(vur[vur$ID == "T2C2", "Response"])
  expect_true(vur[vur$ID == "T2C3", "Response"])
})

test_that('T3',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_3 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_3, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3.RData")

    # change margins to 2,.5,1.5,.25 and output rtf for manual review
    pharmaRTF::margins(test_3) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
    pharmaRTF::write_rtf(test_3, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3.rtf')

    rm(ht)
    rm(test_3)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(test_3))
  testthat::expect_true(vur[vur$ID == "T3C2", "Response"])
  rm(test_3)
})

test_that('T4',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_4 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_4, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_4.RData")

    # change header and footer heights to .25,1 and output rtf for manual review
    pharmaRTF::header_height(test_4) <- .25
    pharmaRTF::footer_height(test_4) <- 1
    pharmaRTF::write_rtf(test_4, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_4.rtf')

    rm(ht)
    rm(test_4)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_4.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(.5, pharmaRTF::header_height(test_4)) %>%
    testthat::expect_equal(.5, pharmaRTF::footer_height(test_4))
  testthat::expect_true(vur[vur$ID == "T4C2", "Response"])
  rm(test_4)
})

test_that('T5',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_5 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_5, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_5.RData")

    # change page size to 9,12 and output rtf for manual review
    pharmaRTF::pagesize(test_5) <- c(height = 9, width = 12)
    pharmaRTF::write_rtf(test_5, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_5.rtf')

    rm(ht)
    rm(test_5)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_5.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(height = 8.5, width = 11), pharmaRTF::pagesize(test_5))
  testthat::expect_true(vur[vur$ID == "T5C2", "Response"])
  rm(test_5)
})

test_that('T6',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_6 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_6, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_6.RData")

    # change orientation to portrait and output rtf for manual review
    pharmaRTF::orientation(test_6) <- "portrait"
    pharmaRTF::write_rtf(test_6, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_6.rtf')

    rm(ht)
    rm(test_6)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_6.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal("landscape",pharmaRTF::orientation(test_6))
  testthat::expect_true(vur[vur$ID == "T6C2", "Response"])
  rm(test_6)
})

test_that('T7',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_7 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_7, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_7.RData")

    # change font size to 14 and output rtf for manual review
    pharmaRTF::font_size(test_7) <- 14
    pharmaRTF::write_rtf(test_7, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_7.rtf')

    rm(ht)
    rm(test_7)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_7.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(12,pharmaRTF::font_size(test_7))
  testthat::expect_true(vur[vur$ID == "T7C2", "Response"])
  rm(test_7)
})

test_that('T8',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_8 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_8, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_8.RData")

    # change font to Comic Sans and output rtf for manual review
    pharmaRTF::font(test_8) <- "Comic Sans"
    pharmaRTF::write_rtf(test_8, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_8.rtf')

    rm(ht)
    rm(test_8)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_8.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal("Courier New",pharmaRTF::font(test_8))
  testthat::expect_true(vur[vur$ID == "T8C2", "Response"])
  rm(test_8)
})

test_that('T9',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_9 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_9, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_9.RData")

    # change header rows to 2 and output rtf for manual review
    pharmaRTF::header_rows(test_9) <- 2
    pharmaRTF::write_rtf(test_9, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_9.rtf')

    rm(ht)
    rm(test_9)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_9.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(1,pharmaRTF::header_rows(test_9))
  testthat::expect_true(vur[vur$ID == "T9C2", "Response"])
  rm(test_9)
})

test_that('T10',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_10 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_10, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_10.RData")

    # change column header buffers to 2,1 and output rtf for manual review
    pharmaRTF::column_header_buffer(test_10) <- c(top = 2, bottom = 1)
    pharmaRTF::write_rtf(test_10, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_10.rtf')

    rm(ht)
    rm(test_10)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_10.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(test_10))
  testthat::expect_true(vur[vur$ID == "T10C2", "Response"])
  rm(test_10)
})

test_that('T11',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )
    test_11 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default margins
    save(test_11, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_11.RData")

    # change ignore cell padding to TRUE and output rtf for manual review
    pharmaRTF::ignore_cell_padding(test_11) <- TRUE
    pharmaRTF::write_rtf(test_11, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_11.rtf')

    rm(ht)
    rm(test_11)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_11.RData")
  }

  # tests
  skip_if(is.null(vur))
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(test_11))
  testthat::expect_true(vur[vur$ID == "T11C2", "Response"])
})


rm(vur)








