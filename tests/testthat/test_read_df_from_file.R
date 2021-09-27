library(huxtable)

test_that("read_titles_from_df works as expected", {

  skip_if_not(require("readxl"))

  example_custom_reader <- function(..., table_number=NULL) {

    # If a column isn't populated then the type may be guessed wrong so force it
    col_types <- c('text', 'numeric', 'text', 'text', 'text', 'text', 'logical', 'logical', 'text')
    # pass through arguments from ...
    df <- readxl::read_excel(..., col_types=col_types)

    # Subset and return that dataframe
    df[df$table_number==table_number, !names(df) == 'table_number']
  }
  ht <- as_hux(mtcars)

  expect_silent(
    doc <- rtf_doc(ht) %>% titles_and_footnotes_from_df(
      from.file='./data/titles.xlsx',
      reader=example_custom_reader,
      table_number='test')
  )

})
