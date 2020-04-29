context("Atorus Validation")

#' @title Test Cases Code
#' @section Updated By:
#' Ashley Tarasiewicz
#' @section Updated Date:
#' 4/28/2020

library(pharmaRTF)
library(huxtable)
library(testthat)
library(tidyverse)

example_custom_reader <- function(test_number=NULL, ...) {

  # Make sure that readxl is installed before
  if (suppressWarnings(!require('readxl'))) {
    stop("This reader requires the package `readxl`. Install using `install.packages('readxl')")
  }

  # If a column isn't populated then the type may be guessed wrong so force it
  col_types <- c('text', 'text', 'text', 'text')
  # pass through arguments from ...
  df <- readxl::read_excel(..., col_types=col_types)

  # Subset and return that dataframe
  df[df$test_number==test_number, !names(df) == 'test_number']
}

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

  expect_true(vur[vur$ID == "T1.1", "Response"])
  expect_true(vur[vur$ID == "T1.2", "Response"])
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

  expect_true(vur[vur$ID == "T2.1", "Response"])
  expect_true(vur[vur$ID == "T2.2", "Response"])
  expect_true(vur[vur$ID == "T2.3", "Response"])
})

test_that('T3',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )

    test_3 <- pharmaRTF::rtf_doc(ht)

    # output dataframe to check default document level margins
    save(test_3, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3.RData")

    # change document level margins to 2,.5,1.5,.25 and output rtf for manual review
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

  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(test_3), label = "T3.1")
  testthat::expect_true(vur[vur$ID == "T3.2", "Response"])

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

    # output dataframe to check default document level header and footer heights
    save(test_4, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_4.RData")

    # change document level header and footer heights to .25,1 and output rtf for manual review
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

  testthat::expect_true(all.equal(c(.5,.5),
    c(pharmaRTF::header_height(test_4),pharmaRTF::header_height(test_4))),
    label = "T4.1")
  testthat::expect_true(vur[vur$ID == "T4.2", "Response"])

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

    # output dataframe to check default document level page size
    save(test_5, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_5.RData")

    # change document level page size to 9,12 and output rtf for manual review
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

  testthat::expect_equal(c(width = 11, height = 8.5), pharmaRTF::pagesize(test_5),label = "T5.1")
  testthat::expect_true(vur[vur$ID == "T5.2", "Response"])

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

    # output dataframe to check default document level orientation
    save(test_6, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_6.RData")

    # change document level orientation to portrait and output rtf for manual review
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

  testthat::expect_equal("landscape",pharmaRTF::orientation(test_6),label = "T6.1")
  testthat::expect_true(vur[vur$ID == "T6.2", "Response"])

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

    # output dataframe to check default document level font size
    save(test_7, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_7.RData")

    # change document level font size to 14 and output rtf for manual review
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

  testthat::expect_equal(12,pharmaRTF::font_size(test_7), label = "T7.1")
  testthat::expect_true(vur[vur$ID == "T7.2", "Response"])

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

    # output dataframe to check default document level font
    save(test_8, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_8.RData")

    # change document level font to Comic Sans and output rtf for manual review
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

  testthat::expect_equal("Courier New",pharmaRTF::font(test_8), label = "T8.1")
  testthat::expect_true(vur[vur$ID == "T8.2", "Response"])

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

    # output dataframe to check default document level header rows
    save(test_9, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_9.RData")

    # change document level header rows to 2 and output rtf for manual review
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

  testthat::expect_equal(1,pharmaRTF::header_rows(test_9), label = "T9.1")
  testthat::expect_true(vur[vur$ID == "T9.2", "Response"])

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

    # output dataframe to check default document level column header buffers
    save(test_10, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_10.RData")

    # change document level column header buffers to 2,1 and output rtf for manual review
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

  testthat::expect_equal(c(top = 0, bottom = 0),pharmaRTF::column_header_buffer(test_10), label = "T10.1")
  testthat::expect_true(vur[vur$ID == "T10.2", "Response"])

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

    # output dataframe to check default document level ignore cell padding
    save(test_11, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_11.RData")

    # change document level ignore cell padding to TRUE and output rtf for manual review
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

  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(test_11), label = "T11.1")
  testthat::expect_true(vur[vur$ID == "T11.2", "Response"])

  rm(test_11)
})

test_that('T12',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # one title and one footnote in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"))
    test_12 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_12, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_12.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_12)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T12.1", "Response"])
})

test_that('T13',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))
    test_13 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_13, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_13.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_13)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T13.1", "Response"])
})

test_that('T14',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # one title and one footnote using add_titles and add_footnotes
    test_14 <- pharmaRTF::rtf_doc(ht)
    test_14 <- pharmaRTF::add_titles(test_14,hf_line("add_titles Title 1"))
    test_14 <- pharmaRTF::add_footnotes(test_14,hf_line("add_footnotes Footnote 1"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_14, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_14.rtf')

    rm(ht)
    rm(test_14)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T14.1", "Response"])
})

test_that('T15',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote using add_titles and add_footnotes
    test_15 <- pharmaRTF::rtf_doc(ht)
    test_15 <- add_titles(test_15,
                            hf_line("add_titles Title 1"),
                            hf_line("add_titles Title 2"),
                            hf_line("add_titles Title 3"))
    test_15 <- add_footnotes(test_15,
                               hf_line("add_footnotes Footnote 1"),
                               hf_line("add_footnotes Footnote 2"),
                               hf_line("add_footnotes Footnote 3"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_15, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_15.rtf')

    rm(ht)
    rm(test_15)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T15.1", "Response"])
})

test_that('T16',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote in the RTF document creation,
    # then replacing the titles and footnotes using add_titles and add_footnotes
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_16 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    test_16 <- add_titles(test_16,
                            hf_line("add_titles Title 1"),
                            hf_line("add_titles Title 2"),
                            hf_line("add_titles Title 3"),replace = TRUE)
    test_16 <- add_footnotes(test_16,
                               hf_line("add_footnotes Footnote 1"),
                               hf_line("add_footnotes Footnote 2"),
                               hf_line("add_footnotes Footnote 3"),replace = TRUE)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_16, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_16.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_16)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T16.1", "Response"])
})

test_that('T17',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote in the RTF document creation,
    # then appending the titles and footnotes using add_titles and add_footnotes
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_17 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    test_17 <- add_titles(test_17,
                            hf_line("add_titles Title 1"),
                            hf_line("add_titles Title 2"),
                            hf_line("add_titles Title 3"))
    test_17 <- add_footnotes(test_17,
                               hf_line("add_footnotes Footnote 1"),
                               hf_line("add_footnotes Footnote 2"),
                               hf_line("add_footnotes Footnote 3"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_17, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_17.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_17)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T17.1", "Response"])
})

test_that('T18',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes created by importing from a separate file using from.file of titles_and_footnotes_from_df
    test_18 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T18')

    # output rtf for manual review
    pharmaRTF::write_rtf(test_18, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_18.rtf')

    rm(ht)
    rm(test_18)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T18.1", "Response"])
})

test_that('T19',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes created by importing from a separate file using from.df of titles_and_footnotes_from_df
    ts_and_fs <- example_custom_reader('~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx', test_number='T19')
    test_19 <- rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(from.df=ts_and_fs)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_19, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_19.rtf')

    rm(ht)
    rm(ts_and_fs)
    rm(test_19)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T19.1", "Response"])
})

test_that('T20',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH in the RTF document creation
    titles <- list(hf_line(c("rtf_doc", "PAGE_FORMAT: Page %s of %s")),
                   hf_line(c("rtf_doc", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                   hf_line(c("rtf_doc", "FILE_PATH: Source: %s")))
    footnotes <- list(hf_line(c("rtf_doc", "PAGE_FORMAT: Page %s")),
                      hf_line(c("rtf_doc", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                      hf_line(c("rtf_doc", "FILE_PATH: Source: %s")))

    test_20 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_20, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_20.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_20)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T20.1", "Response"])
})

test_that('T21',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH using add_titles and add_footnotes
    test_21 <- pharmaRTF::rtf_doc(ht)
    test_21 <- add_titles(test_21,
                            hf_line(c("add_titles", "PAGE_FORMAT: Page %s of %s")),
                            hf_line(c("add_titles", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                            hf_line(c("add_titles", "FILE_PATH: Source: %s")))
    test_21 <- add_footnotes(test_21,
                               hf_line(c("add_footnotes", "PAGE_FORMAT: Page %s")),
                               hf_line(c("add_footnotes", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                               hf_line(c("add_footnotes", "FILE_PATH: Source: %s")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_21, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_21.rtf')

    rm(ht)
    rm(test_21)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T21.1", "Response"])
})

test_that('T22',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH imported from a separate file using a reader
    test_22 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T22')

    # output rtf for manual review
    pharmaRTF::write_rtf(test_22, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_22.rtf')

    rm(ht)
    rm(test_22)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T22.1", "Response"])
})

test_that('T23',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_23 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default bold
    save(test_23, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_23.RData")

    # change some titles/footnotes bold to TRUE and output rtf for manual review
    pharmaRTF::bold(test_23$titles[[1]]) <- TRUE
    pharmaRTF::bold(test_23$titles[[2]]) <- TRUE

    pharmaRTF::bold(test_23$footnotes[[1]]) <- TRUE
    pharmaRTF::bold(test_23$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_23, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_23.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_23)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_23.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::bold(test_23$titles[[1]]),pharmaRTF::bold(test_23$titles[[2]]),pharmaRTF::bold(test_23$titles[[3]]),
    pharmaRTF::bold(test_23$footnotes[[1]]),pharmaRTF::bold(test_23$footnotes[[2]]),pharmaRTF::bold(test_23$footnotes[[3]]))),
    label = "T23.1")
   expect_true(vur[vur$ID == "T23.2", "Response"])

  rm(test_23)
})

test_that('T24',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_24 <- pharmaRTF::rtf_doc(ht)
    test_24 <- add_titles(test_24,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_24 <- add_footnotes(test_24,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line("add_footnotes Footnote 3"))

    # output dataframe to check default bold
    save(test_24, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_24.RData")

    # change some titles/footnotes bold to TRUE and output rtf for manual review
    pharmaRTF::bold(test_24$titles[[1]]) <- TRUE
    pharmaRTF::bold(test_24$titles[[2]]) <- TRUE

    pharmaRTF::bold(test_24$footnotes[[1]]) <- TRUE
    pharmaRTF::bold(test_24$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_24, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_24.rtf')

    rm(ht)
    rm(test_24)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_24.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::bold(test_24$titles[[1]]),pharmaRTF::bold(test_24$titles[[2]]),pharmaRTF::bold(test_24$titles[[3]]),
    pharmaRTF::bold(test_24$footnotes[[1]]),pharmaRTF::bold(test_24$footnotes[[2]]),pharmaRTF::bold(test_24$footnotes[[3]]))),
    label = "T24.1")
  expect_true(vur[vur$ID == "T24.2", "Response"])

  rm(test_24)
})

test_that('T25',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # vtitles/footnotes imported from a separate file using a reader
    test_25 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T25')

    # output dataframe to check default bold
    save(test_25, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_25.RData")

    # change some titles/footnotes bold to TRUE and output rtf for manual review
    pharmaRTF::bold(test_25$titles[[1]]) <- TRUE
    pharmaRTF::bold(test_25$titles[[2]]) <- TRUE

    pharmaRTF::bold(test_25$footnotes[[1]]) <- TRUE
    pharmaRTF::bold(test_25$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_25, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_25.rtf')

    rm(ht)
    rm(test_25)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_25.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::bold(test_25$titles[[1]]),pharmaRTF::bold(test_25$titles[[2]]),pharmaRTF::bold(test_25$titles[[3]]),
    pharmaRTF::bold(test_25$footnotes[[1]]),pharmaRTF::bold(test_25$footnotes[[2]]),pharmaRTF::bold(test_25$footnotes[[3]]))),
    label = "T25.1")
  expect_true(vur[vur$ID == "T25.2", "Response"])

  rm(test_25)
})

test_that('T26',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_26 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default italic
    save(test_26, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_26.RData")

    # change some titles/footnotes italic to TRUE and output rtf for manual review
    pharmaRTF::italic(test_26$titles[[2]]) <- TRUE
    pharmaRTF::italic(test_26$titles[[3]]) <- TRUE

    pharmaRTF::italic(test_26$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_26, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_26.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_26)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_26.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::italic(test_26$titles[[1]]),pharmaRTF::italic(test_26$titles[[2]]),pharmaRTF::italic(test_26$titles[[3]]),
    pharmaRTF::italic(test_26$footnotes[[1]]),pharmaRTF::italic(test_26$footnotes[[2]]),pharmaRTF::italic(test_26$footnotes[[3]]))),
    label = "T26.1")
  expect_true(vur[vur$ID == "T26.2", "Response"])

  rm(test_26)
})

test_that('T27',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_27 <- pharmaRTF::rtf_doc(ht)
    test_27 <- add_titles(test_27,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_27 <- add_footnotes(test_27,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line("add_footnotes Footnote 3"))

    # output dataframe to check default italic
    save(test_27, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_27.RData")

    # change some titles/footnotes italic to TRUE and output rtf for manual review
    pharmaRTF::italic(test_27$titles[[2]]) <- TRUE
    pharmaRTF::italic(test_27$titles[[3]]) <- TRUE

    pharmaRTF::italic(test_27$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_27, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_27.rtf')

    rm(ht)
    rm(test_27)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_27.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::italic(test_27$titles[[1]]),pharmaRTF::italic(test_27$titles[[2]]),pharmaRTF::italic(test_27$titles[[3]]),
    pharmaRTF::italic(test_27$footnotes[[1]]),pharmaRTF::italic(test_27$footnotes[[2]]),pharmaRTF::italic(test_27$footnotes[[3]]))),
    label = "T27.1")
  expect_true(vur[vur$ID == "T27.2", "Response"])

  rm(test_27)
})

test_that('T28',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes imported from a separate file using a reader
    test_28 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T28')

    # output dataframe to check default italic
    save(test_28, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_28.RData")

    # change some titles/footnotes italic to TRUE and output rtf for manual review
    pharmaRTF::italic(test_28$titles[[2]]) <- TRUE
    pharmaRTF::italic(test_28$titles[[3]]) <- TRUE

    pharmaRTF::italic(test_28$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_28, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_28.rtf')

    rm(ht)
    rm(test_28)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_28.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(FALSE,6)),
    c(pharmaRTF::italic(test_28$titles[[1]]),pharmaRTF::italic(test_28$titles[[2]]),pharmaRTF::italic(test_28$titles[[3]]),
    pharmaRTF::italic(test_28$footnotes[[1]]),pharmaRTF::italic(test_28$footnotes[[2]]),pharmaRTF::italic(test_28$footnotes[[3]]))),
    label = "T28.1")
  expect_true(vur[vur$ID == "T28.2", "Response"])

  rm(test_28)
})

test_that('T29',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3 Part 1", "rtf_doc Footnote 3 Part 2"))

    test_29 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default align
    save(test_29, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_29.RData")

    # change some titles/footnotes align and output rtf for manual review
    pharmaRTF::align(test_29$titles[[1]]) <- "left"
    pharmaRTF::align(test_29$titles[[2]]) <- "right"

    pharmaRTF::align(test_29$footnotes[[1]]) <- "left"
    pharmaRTF::align(test_29$footnotes[[2]]) <- "left"
    pharmaRTF::align(test_29$footnotes[[3]]) <- "split"

    pharmaRTF::write_rtf(test_29, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_29.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_29)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_29.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep("center",6)),
    c(pharmaRTF::align(test_29$titles[[1]]),pharmaRTF::align(test_29$titles[[2]]),pharmaRTF::align(test_29$titles[[3]]),
    pharmaRTF::align(test_29$footnotes[[1]]),pharmaRTF::align(test_29$footnotes[[2]]),pharmaRTF::align(test_29$footnotes[[3]]))),
    label = "T29.1")
  expect_true(vur[vur$ID == "T29.2", "Response"])

  rm(test_29)
})

test_that('T30',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_30 <- pharmaRTF::rtf_doc(ht)
    test_30 <- add_titles(test_30,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_30 <- add_footnotes(test_30,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line(c("add_footnotes Footnote 3 Part 1", "add_footnotes Footnote 3 Part 2")))

    # output dataframe to check default align
    save(test_30, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_30.RData")

    # change some titles/footnotes align and output rtf for manual review
    pharmaRTF::align(test_30$titles[[1]]) <- "left"
    pharmaRTF::align(test_30$titles[[2]]) <- "right"

    pharmaRTF::align(test_30$footnotes[[1]]) <- "left"
    pharmaRTF::align(test_30$footnotes[[2]]) <- "left"
    pharmaRTF::align(test_30$footnotes[[3]]) <- "split"

    pharmaRTF::write_rtf(test_30, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_30.rtf')

    rm(ht)
    rm(test_30)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_30.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep("center",6)),
    c(pharmaRTF::align(test_30$titles[[1]]),pharmaRTF::align(test_30$titles[[2]]),pharmaRTF::align(test_30$titles[[3]]),
    pharmaRTF::align(test_30$footnotes[[1]]),pharmaRTF::align(test_30$footnotes[[2]]),pharmaRTF::align(test_30$footnotes[[3]]))),
    label = "T30.1")
  expect_true(vur[vur$ID == "T30.2", "Response"])

  rm(test_30)
})

test_that('T31',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes imported from a separate file using a reader
    test_31 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T31')

    # output dataframe to check default align
    save(test_31, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_31.RData")

    # change some titles/footnotes align and output rtf for manual review
    pharmaRTF::align(test_31$titles[[1]]) <- "left"
    pharmaRTF::align(test_31$titles[[2]]) <- "right"

    pharmaRTF::align(test_31$footnotes[[1]]) <- "left"
    pharmaRTF::align(test_31$footnotes[[2]]) <- "left"
    pharmaRTF::align(test_31$footnotes[[3]]) <- "split"

    pharmaRTF::write_rtf(test_31, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_31.rtf')

    rm(ht)
    rm(test_31)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_31.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep("center",6)),
    c(pharmaRTF::align(test_31$titles[[1]]),pharmaRTF::align(test_31$titles[[2]]),pharmaRTF::align(test_31$titles[[3]]),
    pharmaRTF::align(test_31$footnotes[[1]]),pharmaRTF::align(test_31$footnotes[[2]]),pharmaRTF::align(test_31$footnotes[[3]]))),
    label = "T31.1")
  expect_true(vur[vur$ID == "T31.2", "Response"])

  rm(test_31)
})

test_that('T32',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_32 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default font
    save(test_32, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_32.RData")

    # change some titles/footnotes font to Comic Sans and output rtf for manual review
    pharmaRTF::font(test_32$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_32$titles[[2]]) <- "Comic Sans"

    pharmaRTF::font(test_32$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_32$footnotes[[3]]) <- "Comic Sans"

    pharmaRTF::write_rtf(test_32, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_32.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_32)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_32.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::font(test_32$titles[[1]]),pharmaRTF::font(test_32$titles[[2]]),pharmaRTF::font(test_32$titles[[3]]),
    pharmaRTF::font(test_32$footnotes[[1]]),pharmaRTF::font(test_32$footnotes[[2]]),pharmaRTF::font(test_32$footnotes[[3]]))),
    label = "T32.1")
  expect_true(vur[vur$ID == "T32.2", "Response"])

  rm(test_32)
})

test_that('T33',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_33 <- pharmaRTF::rtf_doc(ht)
    test_33 <- add_titles(test_33,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_33 <- add_footnotes(test_33,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line("add_footnotes Footnote 3"))

    # output dataframe to check default font
    save(test_33, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_33.RData")

    # change some titles/footnotes font to Comic Sans and output rtf for manual review
    pharmaRTF::font(test_33$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_33$titles[[2]]) <- "Comic Sans"

    pharmaRTF::font(test_33$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_33$footnotes[[3]]) <- "Comic Sans"

    pharmaRTF::write_rtf(test_33, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_33.rtf')

    rm(ht)
    rm(test_33)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_33.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::font(test_33$titles[[1]]),pharmaRTF::font(test_33$titles[[2]]),pharmaRTF::font(test_33$titles[[3]]),
    pharmaRTF::font(test_33$footnotes[[1]]),pharmaRTF::font(test_33$footnotes[[2]]),pharmaRTF::font(test_33$footnotes[[3]]))),
    label = "T33.1")
  expect_true(vur[vur$ID == "T33.2", "Response"])

  rm(test_33)
})

test_that('T34',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes imported from a separate file using a reader
    test_34 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T34')

    # output dataframe to check default font
    save(test_34, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_34.RData")

    # change some titles/footnotes font to Comic Sans and output rtf for manual review
    pharmaRTF::font(test_34$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_34$titles[[2]]) <- "Comic Sans"

    pharmaRTF::font(test_34$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font(test_34$footnotes[[3]]) <- "Comic Sans"

    pharmaRTF::write_rtf(test_34, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_34.rtf')

    rm(ht)
    rm(test_34)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_34.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(as.character(NA),6)),
    c(pharmaRTF::font(test_34$titles[[1]]),pharmaRTF::font(test_34$titles[[2]]),pharmaRTF::font(test_34$titles[[3]]),
    pharmaRTF::font(test_34$footnotes[[1]]),pharmaRTF::font(test_34$footnotes[[2]]),pharmaRTF::font(test_34$footnotes[[3]]))),
    label = "T34.1")
  expect_true(vur[vur$ID == "T34.2", "Response"])

  rm(test_34)
})

test_that('T35',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_35 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default font size
    save(test_35, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_35.RData")

    # change some titles/footnotes font size and output rtf for manual review
    pharmaRTF::font_size(test_35$titles[[2]]) <- 14
    pharmaRTF::font_size(test_35$titles[[3]]) <- 10

    pharmaRTF::font_size(test_35$footnotes[[1]]) <- 30

    pharmaRTF::write_rtf(test_35, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_35.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_35)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_35.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::font_size(test_35$titles[[1]]),pharmaRTF::font_size(test_35$titles[[2]]),pharmaRTF::font_size(test_35$titles[[3]]),
    pharmaRTF::font_size(test_35$footnotes[[1]]),pharmaRTF::font_size(test_35$footnotes[[2]]),pharmaRTF::font_size(test_35$footnotes[[3]]))),
    label = "T35.1")
  expect_true(vur[vur$ID == "T35.2", "Response"])

  rm(test_35)
})

test_that('T36',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_36 <- pharmaRTF::rtf_doc(ht)
    test_36 <- add_titles(test_36,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_36 <- add_footnotes(test_36,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line("add_footnotes Footnote 3"))

    # output dataframe to check default font size
    save(test_36, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_36.RData")

    # change some titles/footnotes font size and output rtf for manual review
    pharmaRTF::font_size(test_36$titles[[2]]) <- 14
    pharmaRTF::font_size(test_36$titles[[3]]) <- 10

    pharmaRTF::font_size(test_36$footnotes[[1]]) <- 30

    pharmaRTF::write_rtf(test_36, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_36.rtf')

    rm(ht)
    rm(test_36)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_36.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::font_size(test_36$titles[[1]]),pharmaRTF::font_size(test_36$titles[[2]]),pharmaRTF::font_size(test_36$titles[[3]]),
    pharmaRTF::font_size(test_36$footnotes[[1]]),pharmaRTF::font_size(test_36$footnotes[[2]]),pharmaRTF::font_size(test_36$footnotes[[3]]))),
    label = "T36.1")
  expect_true(vur[vur$ID == "T36.2", "Response"])

  rm(test_36)
})

test_that('T37',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes imported from a separate file using a reader
    test_37 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T37')

    # output dataframe to check default font size
    save(test_37, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.RData")

    # change some titles/footnotes font size and output rtf for manual review
    pharmaRTF::font_size(test_37$titles[[2]]) <- 14
    pharmaRTF::font_size(test_37$titles[[3]]) <- 10

    pharmaRTF::font_size(test_37$footnotes[[1]]) <- 30

    pharmaRTF::write_rtf(test_37, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.rtf')

    rm(ht)
    rm(test_37)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::font_size(test_37$titles[[1]]),pharmaRTF::font_size(test_37$titles[[2]]),pharmaRTF::font_size(test_37$titles[[3]]),
    pharmaRTF::font_size(test_37$footnotes[[1]]),pharmaRTF::font_size(test_37$footnotes[[2]]),pharmaRTF::font_size(test_37$footnotes[[3]]))),
    label = "T37.1")
  expect_true(vur[vur$ID == "T37.2", "Response"])

  rm(test_37)
})

test_that('T38',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_38 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default index
    save(test_38, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_38.RData")

    # change some index and output rtf for manual review
    pharmaRTF::index(test_38$titles[[2]]) <- 2
    pharmaRTF::index(test_38$titles[[3]]) <- 1

    pharmaRTF::index(test_38$footnotes[[1]]) <- 3
    pharmaRTF::index(test_38$footnotes[[2]]) <- 1
    pharmaRTF::index(test_38$footnotes[[3]]) <- 2

    pharmaRTF::write_rtf(test_38, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_38.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_38)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_38.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::index(test_38$titles[[1]]),pharmaRTF::index(test_38$titles[[2]]),pharmaRTF::index(test_38$titles[[3]]),
    pharmaRTF::index(test_38$footnotes[[1]]),pharmaRTF::index(test_38$footnotes[[2]]),pharmaRTF::index(test_38$footnotes[[3]]))),
    label = "T38.1")
  expect_true(vur[vur$ID == "T38.2", "Response"])

  rm(test_38)
})

test_that('T39',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes created using add_titles and add_footnotes
    test_39 <- pharmaRTF::rtf_doc(ht)
    test_39 <- add_titles(test_39,
                          hf_line("add_titles Title 1"),
                          hf_line("add_titles Title 2"),
                          hf_line("add_titles Title 3"))
    test_39 <- add_footnotes(test_39,
                             hf_line("add_footnotes Footnote 1"),
                             hf_line("add_footnotes Footnote 2"),
                             hf_line("add_footnotes Footnote 3"))

    # output dataframe to check default index
    save(test_39, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_39.RData")

    # change some index and output rtf for manual review
    pharmaRTF::index(test_39$titles[[2]]) <- 2
    pharmaRTF::index(test_39$titles[[3]]) <- 1

    pharmaRTF::index(test_39$footnotes[[1]]) <- 3
    pharmaRTF::index(test_39$footnotes[[2]]) <- 1
    pharmaRTF::index(test_39$footnotes[[3]]) <- 2

    pharmaRTF::write_rtf(test_39, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_39.rtf')

    rm(ht)
    rm(test_39)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_39.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::index(test_39$titles[[1]]),pharmaRTF::index(test_39$titles[[2]]),pharmaRTF::index(test_39$titles[[3]]),
    pharmaRTF::index(test_39$footnotes[[1]]),pharmaRTF::index(test_39$footnotes[[2]]),pharmaRTF::index(test_39$footnotes[[3]]))),
    label = "T39.1")
  expect_true(vur[vur$ID == "T39.2", "Response"])

  rm(test_39)
})

test_that('T40',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles/footnotes imported from a separate file using a reader
    test_40 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T40')

    # output dataframe to check default index
    save(test_40, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_40.RData")

    # change some index and output rtf for manual review
    pharmaRTF::index(test_40$titles[[2]]) <- 2
    pharmaRTF::index(test_40$titles[[3]]) <- 1

    pharmaRTF::index(test_40$footnotes[[1]]) <- 3
    pharmaRTF::index(test_40$footnotes[[2]]) <- 1
    pharmaRTF::index(test_40$footnotes[[3]]) <- 2

    pharmaRTF::write_rtf(test_40, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_40.rtf')

    rm(ht)
    rm(test_40)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_40.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(rep(NA,6)),
    c(pharmaRTF::index(test_40$titles[[1]]),pharmaRTF::index(test_40$titles[[2]]),pharmaRTF::index(test_40$titles[[3]]),
    pharmaRTF::index(test_40$footnotes[[1]]),pharmaRTF::index(test_40$footnotes[[2]]),pharmaRTF::index(test_40$footnotes[[3]]))),
    label = "T40.1")
  expect_true(vur[vur$ID == "T40.2", "Response"])

  rm(test_40)
})

test_that('T41',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create title in the RTF document creation then change title attributes
    titles <- list(hf_line("rtf_doc Title 1"))
    test_41 <- pharmaRTF::rtf_doc(ht, titles = titles)

    pharmaRTF::bold(test_41$titles[[1]]) <- TRUE
    pharmaRTF::italic(test_41$titles[[1]]) <- TRUE
    pharmaRTF::align(test_41$titles[[1]]) <- "right"
    pharmaRTF::font(test_41$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_41$titles[[1]]) <- 13
    pharmaRTF::index(test_41$titles[[1]]) <- 4

    # view titles and output to check changes
    view_test_41 <- view_titles(test_41)
    save(view_test_41, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_41.RData")

    rm(ht)
    rm(titles)
    rm(test_41)
    rm(view_test_41)

  # load output for tests
  } else {
  load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_41.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_41$bold[[1]],view_test_41$italic[[1]],view_test_41$align[[1]],
    view_test_41$font[[1]],view_test_41$font_size[[1]],view_test_41$index[[1]])),
    label = "T41.1")

  rm(view_test_41)
 })

test_that('T42',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create footnote in the RTF document creation then change footnote attributes
    footnotes <- list(hf_line("rtf_doc Footnote 1"))
    test_42 <- pharmaRTF::rtf_doc(ht, footnotes = footnotes)

    pharmaRTF::bold(test_42$footnotes[[1]]) <- TRUE
    pharmaRTF::italic(test_42$footnotes[[1]]) <- TRUE
    pharmaRTF::align(test_42$footnotes[[1]]) <- "right"
    pharmaRTF::font(test_42$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_42$footnotes[[1]]) <- 13
    pharmaRTF::index(test_42$footnotes[[1]]) <- 4

    # view footnotes and output to check changes
    view_test_42 <- view_footnotes(test_42)
    save(view_test_42, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_42.RData")

    rm(ht)
    rm(footnotes)
    rm(test_42)
    rm(view_test_42)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_42.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_42$bold[[1]],view_test_42$italic[[1]],view_test_42$align[[1]],
    view_test_42$font[[1]],view_test_42$font_size[[1]],view_test_42$index[[1]])),
    label = "T42.1")

  rm(view_test_42)
})

test_that('T43',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create title using add_titles then change title attributes
    test_43 <- pharmaRTF::rtf_doc(ht)
    test_43 <- pharmaRTF::add_titles(test_43,hf_line("add_titles Title 1"))

    pharmaRTF::bold(test_43$titles[[1]]) <- TRUE
    pharmaRTF::italic(test_43$titles[[1]]) <- TRUE
    pharmaRTF::align(test_43$titles[[1]]) <- "right"
    pharmaRTF::font(test_43$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_43$titles[[1]]) <- 13
    pharmaRTF::index(test_43$titles[[1]]) <- 4

    # view titles and output to check changes
    view_test_43 <- view_titles(test_43)
    save(view_test_43, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_43.RData")

    rm(ht)
    rm(test_43)
    rm(view_test_43)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_43.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_43$bold[[1]],view_test_43$italic[[1]],view_test_43$align[[1]],
    view_test_43$font[[1]],view_test_43$font_size[[1]],view_test_43$index[[1]])),
    label = "T43.1")

  rm(view_test_43)
})

test_that('T44',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create footnote using add_footnotes then change footnote attributes
    test_44 <- pharmaRTF::rtf_doc(ht)
    test_44 <- pharmaRTF::add_footnotes(test_44,hf_line("add_footnotes Title 1"))

    pharmaRTF::bold(test_44$footnotes[[1]]) <- TRUE
    pharmaRTF::italic(test_44$footnotes[[1]]) <- TRUE
    pharmaRTF::align(test_44$footnotes[[1]]) <- "right"
    pharmaRTF::font(test_44$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_44$footnotes[[1]]) <- 13
    pharmaRTF::index(test_44$footnotes[[1]]) <- 4

    # view footnotes and output to check changes
    view_test_44 <- view_footnotes(test_44)
    save(view_test_44, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_44.RData")

    rm(ht)
    rm(test_44)
    rm(view_test_44)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_44.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_44$bold[[1]],view_test_44$italic[[1]],view_test_44$align[[1]],
    view_test_44$font[[1]],view_test_44$font_size[[1]],view_test_44$index[[1]])),
    label = "T44.1")

  rm(view_test_44)
})

test_that('T45',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create title by importing from a separate file using a reader then change title attributes
    test_45 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T45')

    pharmaRTF::bold(test_45$titles[[1]]) <- TRUE
    pharmaRTF::italic(test_45$titles[[1]]) <- TRUE
    pharmaRTF::align(test_45$titles[[1]]) <- "right"
    pharmaRTF::font(test_45$titles[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_45$titles[[1]]) <- 13
    pharmaRTF::index(test_45$titles[[1]]) <- 4

    # view titles and output to check changes
    view_test_45 <- view_titles(test_45)
    save(view_test_45, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_45.RData")

    rm(ht)
    rm(test_45)
    rm(view_test_45)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_45.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_45$bold[[1]],view_test_45$italic[[1]],view_test_45$align[[1]],
    view_test_45$font[[1]],view_test_45$font_size[[1]],view_test_45$index[[1]])),
    label = "T45.1")

  rm(view_test_45)
})

test_that('T46',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # create footnote by importing from a separate file using a reader then change footnote attributes
    test_46 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T46')

    pharmaRTF::bold(test_46$footnotes[[1]]) <- TRUE
    pharmaRTF::italic(test_46$footnotes[[1]]) <- TRUE
    pharmaRTF::align(test_46$footnotes[[1]]) <- "right"
    pharmaRTF::font(test_46$footnotes[[1]]) <- "Comic Sans"
    pharmaRTF::font_size(test_46$footnotes[[1]]) <- 13
    pharmaRTF::index(test_46$footnotes[[1]]) <- 4

    # view footnotes and output to check changes
    view_test_46 <- view_footnotes(test_46)
    save(view_test_46, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_46.RData")

    rm(ht)
    rm(test_46)
    rm(view_test_46)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/view_test_46.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(TRUE, TRUE,"right","Comic Sans",13,4),
    c(view_test_46$bold[[1]],view_test_46$italic[[1]],view_test_46$align[[1]],
    view_test_46$font[[1]],view_test_46$font_size[[1]],view_test_46$index[[1]])),
    label = "T46.1")

  rm(view_test_46)
})

test_that('T47',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::bold(ht)[, 1] <- TRUE
    test_47 <- pharmaRTF::rtf_doc(ht)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_47, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_47.rtf')

    rm(ht)
    rm(test_47)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T47.1", "Response"])
})

test_that('T48',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::italic(ht)[, 1] <- TRUE
    test_48 <- pharmaRTF::rtf_doc(ht)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_48, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_48.rtf')

    rm(ht)
    rm(test_48)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T48.1", "Response"])
})

test_that('T49',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::align(ht)[, 1] <- "right"
    test_49 <- pharmaRTF::rtf_doc(ht)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_49, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_49.rtf')

    rm(ht)
    rm(test_49)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T49.1", "Response"])
})

test_that('T50',{
  # output creation
  if(is.null(vur)) {
    ht_test_50 <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::row_height(ht_test_50) <- c(.08,.08,.08,.08,.08,.08,.08,.08,.08,.08,.08,.08,.08,.16)
    huxtable::valign(ht_test_50)[, 1] <- "bottom"

    test_50 <- pharmaRTF::rtf_doc(ht_test_50)

    # output huxtable table to check table row height
    save(ht_test_50, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_50.RData")

    # output rtf for manual review
    pharmaRTF::write_rtf(test_50, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_50.rtf')

    rm(ht_test_50)
    rm(test_50)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_50.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c("1"=.08,"2"=.08,"3"=.08,"4"=.08,"5"=.08,"6"=.08,"7"=.08,"8"=.08,"9"=.08,"10"=.08,
    "11"=.08,"12"=.08,"13"=.08,"14"=0.16),huxtable::row_height(ht_test_50)),label = "T50.1")
  expect_true(vur[vur$ID == "T50.2", "Response"])
  expect_true(vur[vur$ID == "T50.3", "Response"])

  rm(ht_test_50)
})

test_that('T51',{
  # output creation
  if(is.null(vur)) {
    ht_test_51 <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::width(ht_test_51) <- 1.5
    test_51 <- pharmaRTF::rtf_doc(ht_test_51)

    # output huxtable table to check table width
    save(ht_test_51, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_51.RData")

    # output rtf for manual review
    pharmaRTF::write_rtf(test_51, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_51.rtf')

    rm(ht_test_51)
    rm(test_51)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_51.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_equal(1.5,huxtable::width(ht_test_51), label = "T51.1")
  expect_true(vur[vur$ID == "T51.2", "Response"])

  rm(ht_test_51)
})

test_that('T52',{
  # output creation
  if(is.null(vur)) {
    ht_test_52 <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    huxtable::col_width(ht_test_52) <- c(.4, .8)
    test_52 <- pharmaRTF::rtf_doc(ht_test_52)

    # output huxtable table to check column width
    save(ht_test_52, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_52.RData")

    # output rtf for manual review
    pharmaRTF::write_rtf(test_52, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_52.rtf')

    rm(ht_test_52)
    rm(test_52)

  # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/ht_test_52.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_true(all.equal(c(column1 = 0.4, column2 = 0.8),huxtable::col_width(ht_test_52)),label = "T52.1")
  expect_true(vur[vur$ID == "T52.2", "Response"])

  rm(ht_test_52)
})

test_that('T53',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:13),
      column2 = c("Header2", letters[1:13])
    )
    ht <- huxtable::merge_cells(ht, 2, 1:2)
    test_53 <- pharmaRTF::rtf_doc(ht)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_53, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_53.rtf')

    rm(ht)
    rm(test_53)
  }

  # tests
  skip_if(is.null(vur))

  expect_true(vur[vur$ID == "T53.1", "Response"])
})

rm(vur)


