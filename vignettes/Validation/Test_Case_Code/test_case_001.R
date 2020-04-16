context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)

test_that('T1.01',{

  # dataframe requiring one page of output
  ht <- huxtable::huxtable(
    column1 = c("Header1", 1:10),
    column2 = c("Header2", letters[1:10])
  )

  rtf <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_1_01.rtf')

  rm(ht)
  rm(rtf)
})

test_that('T1.02',{

  # dataframe requiring multiple pages of output
  ht <- huxtable::huxtable(
    column1 = c("Header1", 1:26),
    column2 = c("Header2", letters[1:26])
  )

  rtf <- pharmaRTF::rtf_doc(ht, list(hf_line("Title")), list(hf_line("Footnote")))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_1_02.rtf')

  rm(ht)
  rm(rtf)
})

