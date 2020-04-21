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

ht <- huxtable::huxtable(
  column1 = c("Header1", 1:10),
  column2 = c("Header2", letters[1:10])
)

test_that('T2.01',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.01_a.RData")

    rtf2 <- rtf1
    pharmaRTF::margins(rtf2) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.01_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_01.rtf')
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.01_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.01_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(rtf1), label = "T2.01.1")
  testthat::expect_equal(c(top=2, bottom=.5, left=1.5, right=.25), pharmaRTF::margins(rtf2), label = "T2.01.1")

  testthat::expect_true(vur[vur$ID == "T2.01.02", "Response"], label = "T2.01.2")
})

test_that('T2.02',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.02_a.RData")

    rtf2 <- rtf1
    pharmaRTF::header_height(rtf2) <- .25
    pharmaRTF::footer_height(rtf2) <- 1
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.02_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_02.rtf')
  } else{
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.02_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.02_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(.5, pharmaRTF::header_height(rtf1), label = "T2.01.1")
  testthat::expect_equal(.5, pharmaRTF::footer_height(rtf1), label = "T2.01.1")

  testthat::expect_equal(.25, pharmaRTF::header_height(rtf2), label = "T2.01.2")
  testthat::expect_equal(1, pharmaRTF::footer_height(rtf2), label = "T2.01.2")

  testthat::expect_true(vur[vur$ID == "T2.02.02", "Response"], label = "T2.01.2")
})

test_that('T2.03',{


  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)

    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.03_a.RData")

    rtf2 <- rtf1
    pharmaRTF::pagesize(rtf2) <- c(height = 9, width = 12)
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.03_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_03.rtf')
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.03_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.03_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(c(height = 8.5, width = 11), pharmaRTF::pagesize(rtf1), label = "T2.03.1")
  testthat::expect_equal(c(height = 9, width = 12), pharmaRTF::pagesize(rtf2), label = "T2.03.2")

  testthat::expect_true(vur[vur$ID == "T2.03.02", "Response"], label = "T2.03.2")
})

test_that('T2.04',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)

    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.04_a.RData")

    rtf2 <- rtf1
    pharmaRTF::orientation(rtf2) <- "portrait"
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.04_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_04.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.04_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.04_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal("landscape",pharmaRTF::orientation(rtf1), label = "T2.04.1")
  testthat::expect_equal("portrait",pharmaRTF::orientation(rtf2), label = "T2.04.2")

  testthat::expect_true(vur[vur$ID == "T2.04.02", "Response"], label = "T2.04.2")
})

test_that('T2.05',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.05_a.RData")

    rtf2 <- rtf1
    pharmaRTF::font_size(rtf2) <- 14
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.05_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_05.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.05_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.05_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(12,pharmaRTF::font_size(rtf1), label = "T2.05.1")
  testthat::expect_equal(14,pharmaRTF::font_size(rtf2), label = "T2.05.2")

  testthat::expect_true(vur[vur$ID == "T2.05.02", "Response"], label = "T2.05.2")
})

test_that('T2.06',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.06_a.RData")

    rtf2 <- rtf1
    pharmaRTF::font(rtf2) <- "Comic Sans"
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.06_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_06.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.06_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.06_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal("Courier New",pharmaRTF::font(rtf1), label = "T2.06.1")
  testthat::expect_equal("Comic Sans",pharmaRTF::font(rtf2), label = "T2.06.2")

  testthat::expect_true(vur[vur$ID == "T2.06.02", "Response"], label = "T2.06.2")
})

test_that('T2.07',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.07_a.RData")

    rtf2 <- rtf1
    pharmaRTF::header_rows(rtf2) <- 2
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.07_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_07.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.07_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.07_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(1,pharmaRTF::header_rows(rtf1), label = "T2.07.1")
  testthat::expect_equal(2,pharmaRTF::header_rows(rtf2), label = "T2.07.2")

  testthat::expect_true(vur[vur$ID == "T2.07.02", "Response"], label = "T2.07.2")
})

test_that('T2.08',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.08_a.RData")

    rtf2 <- rtf1
    pharmaRTF::column_header_buffer(rtf2) <- c(top = 2, bottom = 1)
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.08_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_08.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.08_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.08_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(rtf1), label = "T2.08.1")
  testthat::expect_equal(c(top = 2, bottom = 1),pharmaRTF::column_header_buffer(rtf2), label = "T2.08.2")

  testthat::expect_true(vur[vur$ID == "T2.08.02", "Response"], label = "T2.08.2")
})

test_that('T2.09',{

  if(is.null(vur)) {
    rtf1 <- pharmaRTF::rtf_doc(ht)
    save(rtf1, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.09_a.RData")

    rtf2 <- rtf1
    pharmaRTF::ignore_cell_padding(rtf2) <- TRUE
    save(rtf2, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.09_b.RData")

    pharmaRTF::write_rtf(rtf2, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_2_09.rtf')

  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.09_a.RData")
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/Obj/rtf_T2.09_b.RData")
  }
  skip_if(is.null(vur))
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(rtf1), label = "T2.09.1")
  testthat::expect_equal(TRUE,pharmaRTF::ignore_cell_padding(rtf2), label = "T2.09.2")

  testthat::expect_true(vur[vur$ID == "T2.09.02", "Response"], label = "T2.09.2")
})
