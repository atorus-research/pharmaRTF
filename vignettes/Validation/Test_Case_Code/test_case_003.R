context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)

vur <- NULL
if(file.exists("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")) vur <- readRDS("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")

test_that('T3.01',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # one title and one footnote in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"))
    test_3_01 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_01, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_01.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_01)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.01.01", "Response"])
})

test_that('T3.02',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))
    test_3_02 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_02, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_02.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_02)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.02.01", "Response"])
})

test_that('T3.03',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # one title and one footnote using add_titles and add_footnotes
    test_3_03 <- pharmaRTF::rtf_doc(ht)
    test_3_03 <- pharmaRTF::add_titles(test_3_03,hf_line("add_titles Title 1"))
    test_3_03 <- pharmaRTF::add_footnotes(test_3_03,hf_line("add_footnotes Footnote 1"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_03, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_03.rtf')

    rm(ht)
    rm(test_3_03)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.03.01", "Response"])
})

test_that('T3.04',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # more than one title and more than one footnote in the RTF document creation
    test_3_04 <- pharmaRTF::rtf_doc(ht)
    test_3_04 <- add_titles(test_3_04,
                      hf_line("add_titles Title 1"),
                      hf_line("add_titles Title 2"),
                      hf_line("add_titles Title 3"))
    test_3_04 <- add_footnotes(test_3_04,
                         hf_line("add_footnotes Footnote 1"),
                         hf_line("add_footnotes Footnote 2"),
                         hf_line("add_footnotes Footnote 3"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_04, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_04.rtf')

    rm(ht)
    rm(test_3_04)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.04.01", "Response"])
})

test_that('T3.05',{
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

    test_3_05 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    test_3_05 <- add_titles(test_3_05,
                      hf_line("add_titles Title 1"),
                      hf_line("add_titles Title 2"),
                      hf_line("add_titles Title 3"),replace = TRUE)
    test_3_05 <- add_footnotes(test_3_05,
                         hf_line("add_footnotes Footnote 1"),
                         hf_line("add_footnotes Footnote 2"),
                         hf_line("add_footnotes Footnote 3"),replace = TRUE)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_05, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_05.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_05)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.05.01", "Response"])
})

test_that('T3.06',{
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

    test_3_06 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    test_3_06 <- add_titles(test_3_06,
                      hf_line("add_titles Title 1"),
                      hf_line("add_titles Title 2"),
                      hf_line("add_titles Title 3"))
    test_3_06 <- add_footnotes(test_3_06,
                         hf_line("add_footnotes Footnote 1"),
                         hf_line("add_footnotes Footnote 2"),
                         hf_line("add_footnotes Footnote 3"))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_06, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_06.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_06)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.06.01", "Response"])
})



test_that('T3.09',{
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

    test_3_09 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_09, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_09.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_09)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.09.01", "Response"])
})

test_that('T3.10',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH using add_titles and add_footnotes
    test_3_10 <- pharmaRTF::rtf_doc(ht)
    test_3_10 <- add_titles(test_3_10,
                      hf_line(c("add_titles ", "PAGE_FORMAT: Page %s of %s")),
                      hf_line(c("add_titles ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                      hf_line(c("add_titles ", "FILE_PATH: Source: %s")))
    test_3_10 <- add_footnotes(test_3_10,
                         hf_line(c("add_footnotes ", "PAGE_FORMAT: Page %s of %s")),
                         hf_line(c("add_footnotes ", "DATE_FORMAT: %H:%M %A, %B %d, %Y")),
                         hf_line(c("add_footnotes ", "FILE_PATH: Source: %s")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_3_10, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_10.rtf')

    rm(ht)
    rm(test_3_10)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T3.10.01", "Response"])
})





test_that('T3.12',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify bold is set to expected default:FALSE in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_3_12 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default bold
    save(test_3_12, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_12.RData")

    # change bold to TRUE and output rtf for manual review
    pharmaRTF::bold(test_3_12$titles[[1]]) <- TRUE
    pharmaRTF::bold(test_3_12$titles[[2]]) <- TRUE
    pharmaRTF::bold(test_3_12$titles[[3]]) <- TRUE

    pharmaRTF::bold(test_3_12$footnotes[[1]]) <- TRUE
    pharmaRTF::bold(test_3_12$footnotes[[2]]) <- TRUE
    pharmaRTF::bold(test_3_12$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_3_12, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_12.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_12)

  # load output for tests
  } else {
  load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_12.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_12$footnotes[[3]]))
  expect_true(vur[vur$ID == "T3.12.02", "Response"])
})

test_that('T3.13',{
  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    # verify italic is set to expected default:FALSE in the RTF document creation
    titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
    footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

    test_3_13 <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

    # output dataframe to check default bold
    save(test_3_13, file = "~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_13.RData")

    # change italic to TRUE and output rtf for manual review
    pharmaRTF::italic(test_3_13$titles[[1]]) <- TRUE
    pharmaRTF::italic(test_3_13$titles[[2]]) <- TRUE
    pharmaRTF::italic(test_3_13$titles[[3]]) <- TRUE

    pharmaRTF::italic(test_3_13$footnotes[[1]]) <- TRUE
    pharmaRTF::italic(test_3_13$footnotes[[2]]) <- TRUE
    pharmaRTF::italic(test_3_13$footnotes[[3]]) <- TRUE

    pharmaRTF::write_rtf(test_3_13, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_13.rtf')

    rm(ht)
    rm(titles)
    rm(footnotes)
    rm(test_3_13)

    # load output for tests
  } else {
    load("~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_3_13.RData")
  }

  # tests
  skip_if(is.null(vur))

  testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$titles[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$titles[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$titles[[3]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$footnotes[[1]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$footnotes[[2]])) %>%
    testthat::expect_equal(FALSE, pharmaRTF::bold(test_3_13$footnotes[[3]]))
  expect_true(vur[vur$ID == "T3.12.02", "Response"])
})








