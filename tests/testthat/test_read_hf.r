context("read_hf")

## NOTE: reading hf is assumed to be done by the reader so
# that isn't tested here.


test_that("read_hf returns the same data.frame it is passed if it is a valid df", {
  df <- data.frame(
    type = c("footnote", "title"),
    text1 = c("abc", "xyz"),
    text2 = c("abc", "xyz"),
    align = c("abc", "xyz"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    font = c("abc", "xyz"),
    index = c(1, 2),
    stringsAsFactors = FALSE
  )

  expect_silent(read_hf(from=df))
  expect_equal(read_hf(from=df), df)
})


#### Errors ####
test_that("read_hf throws error for bad from/from.file parameters", {
  expect_error(read_hf(from = data.frame(), from.file = "aFile"),
               "One of, and only one of")
  expect_error(read_hf(), "One of, and only one of")
})

test_that("read_hf throws error when presented a from thats not a df", {
  expect_error(read_hf(from = list()), "from does not inherit")
})

test_that("read_hf throws error for invalid from.file/reader", {
  expect_error(read_hf(from.file = "SomeNonexistantFile.txt", reader = "abc"), "Path 'SomeNonexistantFile.txt' does not exist")
  expect_error(read_hf(from.file = "headers1.txt", reader = "abc"), "reader is not a function")
})

test_that("validated_hf_dataframe throws errors for missing columns" ,{
  required_columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "index")
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
  expect_error(validate_hf_dataframe(df, required_columns),
               "Errors were encountered in data.frame validation")
})

test_that("validated_hf_dataframe throws errors for wrong column types" ,{
  required_columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "index")
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
  expect_error(validate_hf_dataframe(df, required_columns),
               "Errors were encountered in data.frame validation")
})

test_that("validated_hf_dataframe throws errors for bad entry in type" ,{
  required_columns <- c("type", "text1", "text2", "align", "bold", "italic", "font", "index")
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
  expect_error(validate_hf_dataframe(df, required_columns),
               "Errors were encountered in data.frame validation")
})

