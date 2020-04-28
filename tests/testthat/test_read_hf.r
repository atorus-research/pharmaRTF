context("read_hf")
library(huxtable)

## NOTE: reading hf is assumed to be done by the reader so
# that isn't tested here.


test_that("read_hf returns the same data.frame it is passed if it is a valid df", {
  df <- data.frame(
    type = c("footnote", "title"),
    text1 = c("abc", "xyz"),
    text2 = c("abc", "xyz"),
    align = c("center", "center"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", "xyz"),
    font_size = c(10, 20),
    index = c(1, 2),
    stringsAsFactors = FALSE
  )

  expect_silent(read_hf(from.df=df))
  expect_equal(read_hf(from.df=df), df)
})

test_that("titles_and_footnotes_from_df can read a data.frame with only text1, and type", {
  df <- data.frame(
    type = c("footnote"),
    text1 = c("abc"),
    stringsAsFactors = FALSE
  )
  ht <- huxtable(
    column1 = 1:5,
    column2 = letters[1:5]
  )
  rtf <- rtf_doc(ht)

  rtf2 <- titles_and_footnotes_from_df(rtf, from.df = df)

  expect_equal(align(rtf2$footnotes[[1]]), "center")
  expect_equal(bold(rtf2$footnotes[[1]]), FALSE)
  expect_equal(italic(rtf2$footnotes[[1]]), FALSE)
  expect_true(is.na(font(rtf2$footnotes[[1]])))
  expect_true(is.na(font_size(rtf2$footnotes[[1]])))
  expect_true(is.na(index(rtf2$footnotes[[1]])))
})

test_that("fill_missing_data fills in data correctly", {
  columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "font_size", "index")
  df <- data.frame(
    type = c("footnote", "title"),
    text1 = c("abc", "xyz"),
    text2 = c(NA , "xyz"),
    align = c(NA, "split"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", NA),
    font_size = c(NA, 20),
    index = c(NA, 2),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    type = c("footnote", "title"),
    text1 = c("abc", "xyz"),
    text2 = c("", "xyz"),
    align = c("center", "split"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", as.character(NA)),
    font_size = c(as.numeric(NA), 20),
    index = c(NA, 2),
    stringsAsFactors = FALSE
  )

  expect_equal(fill_missing_data(df, columns), df2)

})

#### Errors ####
test_that("read_hf throws error for bad from.df/from.file parameters", {
  expect_error(read_hf(from.df = data.frame(), from.file = "aFile"),
               "One of, and only one of")
  expect_error(read_hf(), "One of, and only one of")
})

test_that("read_hf throws error when presented a from.df thats not a df", {
  expect_error(read_hf(from.df = list()), "from.df does not inherit")
})

test_that("read_hf throws error for invalid from.file/reader", {
  expect_error(read_hf(from.file = "SomeNonexistantFile.txt", reader = "abc"), "Path 'SomeNonexistantFile.txt' does not exist")
  expect_error(read_hf(from.file = "test_read_hf.r", reader = "abc"), "reader is not a function")
})

test_that("validated_hf_dataframe throws errors for missing columns" ,{
  required_columns <- c("type", "text1")
  columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "font_size", "index")
  df <- data.frame(
    type = c("footnote", "title"),
    text1 = c("abc", "xyz"),
    text2 = c("abc", "xyz"),
    align = c("abc", "xyz"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  ## Unable to get the full message as it is ecliplesd by the error
  expect_error(validate_hf_dataframe(df, required_columns, columns),
               "Errors were encountered in data.frame validation")
})

test_that("validated_hf_dataframe throws errors for wrong column types" ,{
  required_columns <- c("type", "text1")
  columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "font_size", "index")
  df <- data.frame(
    type = c("footnote", "title"),
    text1 = c(1, 2),
    text2 = c("abc", "xyz"),
    align = c("abc", "xyz"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", "xyz"),
    index = c(1, 2),
    stringsAsFactors = FALSE
  )

  ## Unable to get the full message as it is ecliplesd by the error
  expect_error(validate_hf_dataframe(df, required_columns, columns),
               "Errors were encountered in data.frame validation")
})

test_that("validated_hf_dataframe throws errors for bad entry in type" ,{
  required_columns <- c("type", "text1")
  columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "font_size", "index")
  df <- data.frame(
    type = c("abc", "xyz"),
    text1 = c("abc", "xyz"),
    text2 = c("abc", "xyz"),
    align = c("abc", "xyz"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", "xyz"),
    index = c(1, 2),
    stringsAsFactors = FALSE
  )

  ## Unable to get the full message as it is ecliplesd by the error
  expect_error(validate_hf_dataframe(df, required_columns, columns),
               "Errors were encountered in data.frame validation")
})

