context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29
#
# test_that('T 1.1.1 Opened as expected',{
#
#   library(pharmaRTF)
#   df <- readRDS('vignettes/t14_2_01.rds')
#
#   ht <- huxtable::as_hux(df)
#   huxtable::bottom_border(ht)[1, ] <- 1
#   huxtable::valign(ht)[1, ] <- 'bottom'
#   huxtable::bold(ht)[1, ] <- TRUE
#   huxtable::align(ht)[1, ] <- 'center'
#   huxtable::width(ht) <- 1.5
#   huxtable::escape_contents(ht) <- FALSE
#   huxtable::col_width(ht) <- c(.2, .2, .12, .12, .12, .12, .12)
#   huxtable::bottom_padding(ht) <- 0
#   huxtable::top_padding(ht) <- 0
#   ?hf_line
#
#   doc <- rtf_doc(ht, titles=list(hf_line('My title')))
#
#   write_rtf(doc, file='test_1_1_1.rtf')
#
#   expect_true(FALSE)
#
# })
#
# test_that('T 1.1.2 Opened as expected also',{
#
#   expect_true(FALSE)
#
# })
