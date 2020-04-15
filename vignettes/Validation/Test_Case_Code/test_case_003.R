context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

library(pharmaRTF)
library(huxtable)

ht <- huxtable::huxtable(
  column1 = c("Header1", 1:26),
  column2 = c("Header2", letters[1:26])
)

test_that('T 3.1.1',{

  # one title and one footnote in the RTF document creation
  titles <- list(hf_line("Title 1"))
  footnotes <- list(hf_line("Footnote 1"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_1.rtf')

  #expect_true(FALSE)

  rm(titles)
  rm(footnotes)
  rm(rtf)
})

test_that('T 3.1.2',{

  # more than one title and more than one footnote in the RTF document creation

  # what makes this need to be in a list but below doesnt??
  titles <- list(hf_line("Title 1"), hf_line("Title 2"), hf_line("Title 3"))
  footnotes <- list(hf_line("Footnote 1"), hf_line("Footnote 2"), hf_line("Footnote 3"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_2.rtf')

  #expect_true(FALSE)

  rm(titles)
  rm(footnotes)
  rm(rtf)
})

test_that('T 3.1.3',{

  # one title and one footnote using add_titles and add_footnotes
  rtf <- pharmaRTF::rtf_doc(ht)
  rtf <- pharmaRTF::add_titles(rtf,hf_line("Title 1"))
  rtf <- pharmaRTF::add_footnotes(rtf,hf_line("Footnote 1"))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_3.rtf')

  #expect_true(FALSE)

  rm(rtf)
})

test_that('T 3.1.4',{

  # more than one title and more than one footnote in the RTF document creation
  rtf <- pharmaRTF::rtf_doc(ht)
  rtf <- add_titles(rtf,
                       hf_line("Title 1"),
                       hf_line("Title 2"),
                       hf_line("Title 3"))
  rtf <- add_footnotes(rtf,
                       hf_line("Footnote 1"),
                       hf_line("Footnote 2"),
                       hf_line("Footnote 3"))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_4.rtf')

  #expect_true(FALSE)

  rm(rtf)
})

test_that('T 3.1.5',{

  # more than one title and more than one footnote in the RTF document creation,
  # then replacing the titles and footnotes using add_titles and add_footnotes
  titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
  footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

  rtf <- add_titles(rtf,
                    hf_line("add_titles Title 1"),
                    hf_line("add_titles Title 2"),
                    hf_line("add_titles Title 3"),replace = TRUE)
  rtf <- add_footnotes(rtf,
                       hf_line("add_footnotes Footnote 1"),
                       hf_line("add_footnotes Footnote 2"),
                       hf_line("add_footnotes Footnote 3"),replace = TRUE)

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_5.rtf')

  #expect_true(FALSE)

  rm(titles)
  rm(footnotes)
  rm(rtf)
})

test_that('T 3.1.6',{

  # more than one title and more than one footnote in the RTF document creation,
  # then appending the titles and footnotes using add_titles and add_footnotes
  titles <- list(hf_line("rtf_doc Title 1"), hf_line("rtf_doc Title 2"), hf_line("rtf_doc Title 3"))
  footnotes <- list(hf_line("rtf_doc Footnote 1"), hf_line("rtf_doc Footnote 2"), hf_line("rtf_doc Footnote 3"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

  rtf <- add_titles(rtf,
                    hf_line("add_titles Title 1"),
                    hf_line("add_titles Title 2"),
                    hf_line("add_titles Title 3"))
  rtf <- add_footnotes(rtf,
                       hf_line("add_footnotes Footnote 1"),
                       hf_line("add_footnotes Footnote 2"),
                       hf_line("add_footnotes Footnote 3"))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_1_6.rtf')

  #expect_true(FALSE)

  rm(titles)
  rm(footnotes)
  rm(rtf)
})

test_that('T 3.2.1',{

  # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH in the RTF document creation
  titles <- list(hf_line("PAGE_FORMAT: Page %s of %s"), hf_line("DATE_FORMAT: %H:%M %A, %B %d, %Y"), hf_line("FILE_PATH: Source: %s"))
  footnotes <- list(hf_line("PAGE_FORMAT: Page %s of %s"), hf_line("DATE_FORMAT: %H:%M %A, %B %d, %Y"), hf_line("FILE_PATH: Source: %s"))

  rtf <- pharmaRTF::rtf_doc(ht, titles = titles, footnotes = footnotes)

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_2_1.rtf')

  #expect_true(FALSE)

  rm(titles)
  rm(footnotes)
  rm(rtf)
})

test_that('T 3.2.2',{

  # titles and footnotes capturing PAGE_FORMAT DATE_FORMAT and FILE_PATH using add_titles and add_footnotes
  rtf <- pharmaRTF::rtf_doc(ht)
  rtf <- add_titles(rtf,
                       hf_line("PAGE_FORMAT: Page %s of %s"),
                       hf_line("DATE_FORMAT: %H:%M %A, %B %d, %Y"),
                       hf_line("FILE_PATH: Source: %s"))
  rtf <- add_footnotes(rtf,
                       hf_line("PAGE_FORMAT: Page %s of %s"),
                       hf_line("DATE_FORMAT: %H:%M %A, %B %d, %Y"),
                       hf_line("FILE_PATH: Source: %s"))

  # write to rtf
  pharmaRTF::write_rtf(rtf, file='test_3_2_2.rtf')

  #expect_true(FALSE)

  rm(rtf)
})






test_that('T 3.3.1',{

  rtf <- pharmaRTF::rtf_doc(ht)

  # Verify margins are set to expected default:1,1,1,1
  testthat::expect_equal(c(top=1, bottom=1, left=1, right=1), pharmaRTF::margins(rtf))

  # Change margins to 2,.5,1.5,.25 and check output to verify they have been changed
  pharmaRTF::margins(rtf) <- c(top = 2, bottom = .5, left = 1.5, right = .25)
  pharmaRTF::write_rtf(rtf, file='test_2_1_1.rtf')

  #expect_true(FALSE)

  rm(rtf)
})


