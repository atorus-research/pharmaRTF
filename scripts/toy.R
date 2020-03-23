# Toy example
library(assertthat)
library(huxtable)

ht <- as_hux(cars, add_colnames=T)
doc <- as_rtf_doc(ht)

doc <- add_hf(doc,
              hf_line('Protocol something', 'PAGE_FORMAT: Page %s of %s', align='split', index=1),
              hf_line('line 4a', 'line 4b', align='split', index=2, font='Helvetica'),
              hf_line('line 5', index=4),
              hf_line('line 3', index=3), to='titles', replace=TRUE)

write_rtf(doc)
