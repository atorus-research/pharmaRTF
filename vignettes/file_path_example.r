library(pharmaRTF)
library(huxtable)

# Create the column headers data frame
column_headers <- data.frame(
  Species = c("", "Species of Flower"),
  Sepal.Length = c("Sepal", "Length"),
  Sepal.Width = c("", "Width"),
  Petal.Length = c("Petal", "Length"),
  Petal.Width = c("", "Width"),
  stringsAsFactors = FALSE
)

# Create the huxtable table
ht <- huxtable::as_hux(rbind(column_headers, iris)) %>%
  # Merge the Sepal cell over the Length and Width
  huxtable::merge_cells(1, 2:3) %>%
  # Merge the Petal cell over the Length and Width
  huxtable::merge_cells(1, 4:5) %>%
  # Align the top cells for both Sepal and Petal
  huxtable::set_align(1,2, 'center') %>%
  huxtable::set_align(1,4, 'center') %>%
  # Bold all the column header rows
  huxtable::set_bold(1:2, 1:ncol(iris), TRUE) %>%
  # Bottom border on 1st column header row
  huxtable::set_bottom_border(1, 2:4, 1) %>%
  # Bottom border on 2nd row
  huxtable::set_bottom_border(2, 1:ncol(iris), 2) %>%
  # Set the page width
  huxtable::set_width(1.5)

# Create the RTF doc
doc <- rtf_doc(ht, header_rows=2) %>%
  add_titles(hf_line("Text on the left", "FILE_PATH: Run from: %s", bold=TRUE, align='split'))


write_rtf(doc, file="table15.rtf")
