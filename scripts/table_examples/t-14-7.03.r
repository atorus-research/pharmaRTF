### Table 14-7.03 pg 148 Summary of Weight Change from Baseline at End of Treatment

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

## FIXME: Not getting the correct counts for WEEK 26(End of Trt.). write logic to determine last VISITDY
vs <- read_xpt(glue("{sdtm_lib}/vs.xpt")) %>%
  filter(VSTESTCD == "WEIGHT" & VISIT %in% c("BASELINE", "WEEK 24", "WEEK 26"))
dm <- read_xpt(glue("{sdtm_lib}/dm.xpt"))


vs_1 <- vs %>%
  merge(dm[,c("USUBJID", "ARM")], by = "USUBJID")

bw_stats <- vs_1 %>%
  group_by(ARM, VISIT) %>%
  summarise(n = n(),
            Mean = mean(VSSTRESN),
            SD = sd(VSSTRESN),
            Median = median(VSSTRESN),
            Min. = min(VSSTRESN),
            Max. = max(VSSTRESN))



header <- c(
  "Measure",
  "Treatment",
  "Planned Relative Time",
  names(bw_stats)
)



ht <- bw_stats %>%
  huxtable::as_hux(add_colnames=TRUE)


huxtable::bottom_border(ht)[1, ] <- 1
huxtable::bold(ht)[1, ] <- TRUE
huxtable::align(ht)[1, ] <- 'center'
huxtable::width(ht) <- 1.5
huxtable::escape_contents(ht) <- FALSE
huxtable::bottom_padding(ht) <- 0
huxtable::top_padding(ht) <- 0




# Write into doc object and pull titles/footnotes from excel file
doc <- as_rtf_doc(ht) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-7.03') %>%
  set_font_size(10)

write_rtf(doc, file='./scripts/table_examples/outputs/14-7.03.rtf')
