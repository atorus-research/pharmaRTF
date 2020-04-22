context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)
library(testthat)

example_custom_reader <- function(..., test_number=NULL) {

  # Make sure that readxl is installed before
  if (suppressWarnings(!require('readxl'))) {
    stop("This reader requires the package `readxl`. Install using `install.packages('readxl')")
  }

  # If a column isn't populated then the type may be guessed wrong so force it
  col_types <- c('text', 'numeric', 'text', 'text', 'text', 'text', 'logical', 'logical', 'text')
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
  testthat::expect_equal(FALSE,pharmaRTF::ignore_cell_padding(test_11))
  testthat::expect_true(vur[vur$ID == "T11C2", "Response"])
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
  expect_true(vur[vur$ID == "T12C1", "Response"])
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
  expect_true(vur[vur$ID == "T13C1", "Response"])
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
  expect_true(vur[vur$ID == "T14C1", "Response"])
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
  expect_true(vur[vur$ID == "T15C1", "Response"])
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

    test_16 <- add_titles(test_316,
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
  expect_true(vur[vur$ID == "T16C1", "Response"])
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
  expect_true(vur[vur$ID == "T17C1", "Response"])
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
  expect_true(vur[vur$ID == "T18C1", "Response"])
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
  expect_true(vur[vur$ID == "T19C1", "Response"])
})

test_that('T20',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH in the RTF document creation
    titles <- list(hf_line(c("rtf_doc ", "PAGE_FORMAT: Page %s of %s")),
                   hf_line(c("rtf_doc ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                   hf_line(c("rtf_doc ", "FILE_PATH: Source: %s")))
    footnotes <- list(hf_line(c("rtf_doc ", "PAGE_FORMAT: Page %s of %s")),
                      hf_line(c("rtf_doc ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                      hf_line(c("rtf_doc ", "FILE_PATH: Source: %s")))

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
  expect_true(vur[vur$ID == "T20C1", "Response"])
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
                            hf_line(c("add_titles ", "PAGE_FORMAT: Page %s of %s")),
                            hf_line(c("add_titles ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                            hf_line(c("add_titles ", "FILE_PATH: Source: %s")))
    test_21 <- add_footnotes(test_21,
                               hf_line(c("add_footnotes ", "PAGE_FORMAT: Page %s of %s")),
                               hf_line(c("add_footnotes ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                               hf_line(c("add_footnotes ", "FILE_PATH: Source: %s")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_21, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_21.rtf')

    rm(ht)
    rm(test_21)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T21C1", "Response"])
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
  expect_true(vur[vur$ID == "T22C1", "Response"])
})

test_that('T23',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify bold attribute of titles/footnotes created in the RTF document creation is set to expected default:FALSE
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

  testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_23$footnotes[[3]]))
  expect_true(vur[vur$ID == "T23C2", "Response"])
  rm(test_23)
})

test_that('T24',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify bold attribute of titles/footnotes created using add_titles and add_footnotes is set to expected default:FALSE
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

  testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_24$footnotes[[3]]))
  expect_true(vur[vur$ID == "T24C2", "Response"])
  rm(test_24)
})

test_that('T25',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify bold attribute of titles/footnotes imported from a separate file using a reader is set to file value:FALSE
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

  testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_25$footnotes[[3]]))
  expect_true(vur[vur$ID == "T25C2", "Response"])
  rm(test_25)
})

test_that('T26',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify italic attribute of titles/footnotes created in the RTF document creation is set to expected default:FALSE
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

  testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_26$footnotes[[3]]))
  expect_true(vur[vur$ID == "T26C2", "Response"])
  rm(test_26)
})

test_that('T27',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify italic attribute of titles/footnotes created using add_titles and add_footnotes is set to expected default:FALSE
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

  testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_27$footnotes[[3]]))
  expect_true(vur[vur$ID == "T27C2", "Response"])
  rm(test_27)
})

test_that('T28',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify italic attribute of titles/footnotes imported from a separate file using a reader is set to file value:FALSE
    test_28 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T28')

    # output dataframe to check default bold
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

  testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::italic(test_28$footnotes[[3]]))
  expect_true(vur[vur$ID == "T28C2", "Response"])
  rm(test_28)
})

test_that('T29',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )
    # verify align attribute of titles/footnotes created in the RTF document creation is set to expected default:center
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

  testthat::expect_equal("center", pharmaRTF::align(test_29$titles[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_29$titles[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_29$titles[[3]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_29$footnotes[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_29$footnotes[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_29$footnotes[[3]]))
  expect_true(vur[vur$ID == "T29C2", "Response"])
  rm(test_29)
})

test_that('T30',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )
    # verify align attribute of titles/footnotes created using add_titles and add_footnotes is set to expected default:center
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

  testthat::expect_equal("center", pharmaRTF::align(test_30$titles[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_30$titles[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_30$titles[[3]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_30$footnotes[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_30$footnotes[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_30$footnotes[[3]]))
  expect_true(vur[vur$ID == "T30C2", "Response"])
  rm(test_30)
})

test_that('T31',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )
    # verify align attribute of titles/footnotes imported from a separate file using a reader is set to file value:center
    test_31 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T31')

    # output dataframe to check default bold
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

  testthat::expect_equal("center", pharmaRTF::align(test_31$titles[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_31$titles[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_31$titles[[3]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_31$footnotes[[1]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_31$footnotes[[2]])) %>%
    testthat::expect_equal("center", pharmaRTF::align(test_31$footnotes[[3]]))
  expect_true(vur[vur$ID == "T31C2", "Response"])
  rm(test_31)
})

test_that('T32',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify font attribute of titles/footnotes created in the RTF document creation is set to expected default:Courier New
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

  testthat::expect_equal("Courier New", pharmaRTF::font(test_32$titles[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_32$titles[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_32$titles[[3]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_32$footnotes[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_32$footnotes[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_32$footnotes[[3]]))
  expect_true(vur[vur$ID == "T32C2", "Response"])
  rm(test_32)
})

test_that('T33',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify font attribute of titles/footnotes created using add_titles and add_footnotes is set to expected default:Courier New
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

  testthat::expect_equal("Courier New", pharmaRTF::font(test_33$titles[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_33$titles[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_33$titles[[3]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_33$footnotes[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_33$footnotes[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_33$footnotes[[3]]))
  expect_true(vur[vur$ID == "T33C2", "Response"])
  rm(test_33)
})

test_that('T34',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )
    # verify font attribute of titles/footnotes imported from a separate file using a reader is set to expected default:Courier New
    test_34 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T34')

    # output dataframe to check default bold
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

  testthat::expect_equal("Courier New", pharmaRTF::font(test_34$titles[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_34$titles[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_34$titles[[3]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_34$footnotes[[1]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_34$footnotes[[2]])) %>%
    testthat::expect_equal("Courier New", pharmaRTF::font(test_34$footnotes[[3]]))
  expect_true(vur[vur$ID == "T34C2", "Response"])
  rm(test_34)
})

test_that('T35',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify font size attribute of titles/footnotes created in the RTF document creation is set to expected default:12
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_35 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default font size
    save(test_35, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_35.RData")

    # change some titles/footnotes font size and output rtf for manual review
    pharmaRTF::font_size(test_35$titles[[2]]) <- 14
    pharmaRTF::font_size(test_35$titles[[3]]) <- 10

    pharmaRTF::font_size(test_35$footnotes[[1]]) <- 20

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

  testthat::expect_equal(12, pharmaRTF::font_size(test_35$titles[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_35$titles[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_35$titles[[3]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_35$footnotes[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_35$footnotes[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_35$footnotes[[3]]))
  expect_true(vur[vur$ID == "T35C2", "Response"])
  rm(test_35)
})

test_that('T36',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify font size attribute of titles/footnotes created using add_titles and add_footnotes is set to expected default:12
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

    pharmaRTF::font_size(test_36$footnotes[[1]]) <- 20

    pharmaRTF::write_rtf(test_36, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_36.rtf')

    rm(ht)
    rm(test_36)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_36.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_equal(12, pharmaRTF::font_size(test_36$titles[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_36$titles[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_36$titles[[3]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_36$footnotes[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_36$footnotes[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_36$footnotes[[3]]))
  expect_true(vur[vur$ID == "T36C2", "Response"])
  rm(test_36)
})

test_that('T37',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )
    # verify font size attribute of titles/footnotes imported from a separate file using a reader is set to expected default:12
    test_37 <- pharmaRTF::rtf_doc(ht) %>% pharmaRTF::titles_and_footnotes_from_df(
      from.file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/input/titles_footnotes.xlsx',
      reader=example_custom_reader,
      test_number='T37')

    # output dataframe to check default bold
    save(test_37, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.RData")

    # change some titles/footnotes font size and output rtf for manual review
    pharmaRTF::font_size(test_37$titles[[2]]) <- 14
    pharmaRTF::font_size(test_37$titles[[3]]) <- 10

    pharmaRTF::font_size(test_37$footnotes[[1]]) <- 20

    pharmaRTF::write_rtf(test_37, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.rtf')

    rm(ht)
    rm(test_37)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_37.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_equal(12, pharmaRTF::font_size(test_37$titles[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_37$titles[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_37$titles[[3]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_37$footnotes[[1]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_37$footnotes[[2]])) %>%
    testthat::expect_equal(12, pharmaRTF::font_size(test_37$footnotes[[3]]))
  expect_true(vur[vur$ID == "T37C2", "Response"])
  rm(test_37)
})

test_that('T38',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify index attribute of titles/footnotes created in the RTF document creation is set to expected default:NULL
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

  testthat::expect_equal(NULL, pharmaRTF::index(test_38$titles[[1]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_38$titles[[2]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_38$titles[[3]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_38$footnotes[[1]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_38$footnotes[[2]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_38$footnotes[[3]]))
  expect_true(vur[vur$ID == "T38C2", "Response"])
  rm(test_38)
})

test_that('T39',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify index attribute of titles/footnotes created using add_titles and add_footnotesis set to expected default:NULL
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

  testthat::expect_equal(NULL, pharmaRTF::index(test_39$titles[[1]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_39$titles[[2]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_39$titles[[3]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_39$footnotes[[1]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_39$footnotes[[2]])) %>%
    testthat::expect_equal(NULL, pharmaRTF::index(test_39$footnotes[[3]]))
  expect_true(vur[vur$ID == "T39C2", "Response"])
  rm(test_39)
})

test_that('T40',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify index attribute of titles/footnotes imported from a separate file using a reader is set to file value:1 2 3 1 2 3
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

  testthat::expect_equal(1, pharmaRTF::index(test_40$titles[[1]])) %>%
    testthat::expect_equal(2, pharmaRTF::index(test_40$titles[[2]])) %>%
    testthat::expect_equal(3, pharmaRTF::index(test_40$titles[[3]])) %>%
    testthat::expect_equal(1, pharmaRTF::index(test_40$footnotes[[1]])) %>%
    testthat::expect_equal(2, pharmaRTF::index(test_40$footnotes[[2]])) %>%
    testthat::expect_equal(3, pharmaRTF::index(test_40$footnotes[[3]]))
  expect_true(vur[vur$ID == "T40C2", "Response"])
  rm(test_40)
})



rm(vur)








