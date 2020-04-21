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

test_that('T1.01',{

  # output creation
  if(is.null(vur)) {
    # dataframe requiring one page of output
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:10),
      column2 = c("Header2", letters[1:10])
    )

    test_1_01 <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_1_01, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_1_01.rtf')

    rm(ht)
    rm(test_1_01)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T1.01.01", "Response"])
  expect_true(vur[vur$ID == "T1.01.02", "Response"])
})

test_that('T1.02',{

  # output creation
  if(is.null(vur)) {
    ht <- huxtable::huxtable(
      column1 = c("Header1", 1:26),
      column2 = c("Header2", letters[1:26])
    )

    test_1_02 <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

    # output rtf for manual review
    pharmaRTF::write_rtf(test_1_02, file='~/pharmaRTF/vignettes/Validation/Test_Case_Code/output/test_1_02.rtf')

    rm(ht)
    rm(test_1_02)
  }

  # tests
  skip_if(is.null(vur))
  expect_true(vur[vur$ID == "T1.02.01", "Response"])
  expect_true(vur[vur$ID == "T1.02.02", "Response"])
  expect_true(vur[vur$ID == "T1.02.03", "Response"])
})

rm(vur)







