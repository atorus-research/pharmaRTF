### Table 14-7.03 pg 148 Summary of Weight Change from Baseline at End of Treatment

library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)
library(tibble)


source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

## FIXME: Not getting the correct counts for WEEK 26(End of Trt.). write logic to determine last VISITDY
vs <- read_xpt(glue("{sdtm_lib}/vs.xpt")) %>%
  filter(VSTESTCD == "WEIGHT")
# Logic for End of TRT
vs$EOTFL <- unlist(dlply(vs,
      "USUBJID",
      function(x) {
        ifelse(x$VSDY == max(x$VSDY), "Y", NA)
      }))
vs <- vs %>%
  filter(VISIT %in% c("BASELINE", "WEEK 24") | EOTFL == "Y")
## FIXME: I don't think I can assume this
vs[vs$EOTFL %in% "Y", "VISIT"] <- "End of Trt."
dm <- read_xpt(glue("{sdtm_lib}/dm.xpt"))

# Merge in Arm Data
vs_1 <- vs %>%
  merge(dm[,c("USUBJID", "ARM")], by = "USUBJID")
## Add ordered factor to order arms
vs_1$ARM <- ordered(vs_1$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

# Create table for stats
bw_stats <- vs_1 %>%
  group_by(ARM, VISIT) %>%
  summarise(n = n(),
            Mean = mean(VSSTRESN),
            SD = sd(VSSTRESN),
            Median = median(VSSTRESN),
            Min. = min(VSSTRESN),
            Max. = max(VSSTRESN))
bw_stats <- add_column(bw_stats, "Measure" = "Weight (kg)", .before= 1)
bw_stats[bw_stats$VISIT != "BASELINE", "ARM"] <- NA
bw_stats[!(bw_stats$ARM %in% "Placebo"), "Measure"] <- NA


# Create table for baseline changes
bw_bl <- ddply(vs_1,
                "USUBJID",
                .fun = function(x) {
                  bl <- x[x$VISIT == "BASELINE", "VSSTRESN"]
                  w24 <- x[x$VISIT == "WEEK 24", "VSSTRESN"]
                  eot <- x[x$VISIT == "End of Trt.", "VSSTRESN"]
                  arm <- unique(x$ARM)
                  print(x$USUBJID)
                  print(bl)
                  print(w24)
                  print(eot)
                  ## Done this way to make dplyr easier
                  data.frame(
                    ARM = arm,
                    change = c(length(w24-bl) == 0, NA, w24-bl,
                               ifelse(length(eot-bl) == 0, NA, eot-bl)),
                    VISIT = c("WEEK 24", "End of Trt")
                  )
                }, .inform = TRUE)
## Add ordered factor to order arms
bw_bl$ARM <- ordered(bw_bl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

bw_bl_1 <- bw_bl %>%
  group_by(ARM, VISIT) %>%
  summarise(n = sum(!is.na(change)),
            Mean = mean(change, na.rm = TRUE),
            SD = sd(change, na.rm = TRUE),
            Median = median(change, na.rm = TRUE),
            Min. = min(change, na.rm = TRUE),
            Max. = max(change, na.rm = TRUE))
bw_bl_1 <- add_column(bw_bl_1, "Measure" = "Weight Change from Baseline", .before = 1)
bw_bl_1[bw_bl_1$VISIT != "w24", "ARM"] <- NA
bw_bl_1[!(bw_bl_1$ARM %in% "Placebo"), "Measure"] <- NA


combinedTable <- rbind(bw_stats, bw_bl_1)




ht <- combinedTable %>%
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
