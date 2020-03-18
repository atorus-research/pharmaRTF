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

vs <- read_xpt(glue("{sdtm_lib}/vs.xpt")) %>%
  filter(VSTESTCD == "WEIGHT")
# Logic for End of TRT
vs_eot <- ddply(vs,
                "USUBJID",
                function(x) {
                  x <- x[x$VISITDY <= 168 & x$VSBLFL != "Y",]
                  if(max(x$VSDY) < 0) return()
                  x[x$VSDY == max(x$VSDY),]
                })
vs_eot[,"VISIT"] <- "End of Trt."
## Bind EOT and other visits, some may be the same visit
vs <- vs %>%
  filter(VISIT %in% c("BASELINE", "WEEK 24")) %>%
  rbind(vs_eot)

dm <- read_xpt(glue("{sdtm_lib}/dm.xpt"))

# Merge in Arm Data
vs_1 <- vs %>%
  merge(dm[,c("USUBJID", "ARM")], by = "USUBJID")
## Add ordered factor to order arms
vs_1$ARM <- ordered(vs_1$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
## Add ordered VISITS to order visits
vs_1$VISIT <- ordered(vs_1$VISIT, c("BASELINE", "WEEK 24", "End of Trt."))

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
bw_stats <- add_column(bw_stats, "N" = apply(bw_stats,
                           1,
                           function(x) {aSum <- sum(dm[,"ARM"] == x["ARM"], na.rm = TRUE)
                           ifelse(aSum == 0, NA, aSum)}),
           .before = 3)
# Pad blank rows after End of Trt. rows
pad_row <- function(df, r) {
  #df - dataframe to insert pad
  #r - row number to pad
  for(i in seq(along = r)) {
    if(r[i] + i - 1 < nrow(df)){
      df[seq(r[i] + i, nrow(df) + 1),] <- df[seq(r[i] + (i - 1), nrow(df)),]
      df[r[i] + (i - 1),] <- NA
    } else {
      df[r[i] + (i - 1),] <- NA
    }
  }
  df
}
bw_stats <- pad_row(bw_stats, which(bw_stats$VISIT == "End of Trt.",) + 1)


### Weight Change from Baseline table
# Create table for baseline changes
bw_bl <- ddply(vs_1,
                "USUBJID",
                .fun = function(x) {
                  bl <- x[x$VISIT == "BASELINE", "VSSTRESN"]
                  w24 <- x[x$VISIT == "WEEK 24", "VSSTRESN"]
                  eot <- x[x$VISIT == "End of Trt.", "VSSTRESN"]
                  arm <- unique(x$ARM)
                  ## Done this way to make dplyr easier
                  data.frame(
                    ARM = arm,
                    change = c(ifelse(length(w24-bl) == 0, NA, w24-bl),
                               ifelse(length(eot-bl) == 0, NA, eot-bl)),
                    VISIT = c("WEEK 24", "End of Trt.")
                  )
                })
## Add ordered factor to order arms
bw_bl$ARM <- ordered(bw_bl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
bw_bl$VISIT <- ordered(bw_bl$VISIT,c("WEEK 24", "End of Trt."))

bw_bl_1 <- bw_bl %>%
  group_by(ARM, VISIT) %>%
  summarise(n = sum(!is.na(change)),
            Mean = mean(change, na.rm = TRUE),
            SD = sd(change, na.rm = TRUE),
            Median = median(change, na.rm = TRUE),
            Min. = min(change, na.rm = TRUE),
            Max. = max(change, na.rm = TRUE))
bw_bl_1 <- add_column(bw_bl_1, "Measure" = "Weight Change from Baseline", .before = 1)
bw_bl_1[bw_bl_1$VISIT != "WEEK 24", "ARM"] <- NA
bw_bl_1[!(bw_bl_1$ARM %in% "Placebo"), "Measure"] <- NA

bw_bl_1 <- add_column(bw_bl_1, "N" = apply(bw_bl_1,
                                             1,
                                             function(x) {aSum <- sum(dm[,"ARM"] == x["ARM"], na.rm = TRUE)
                                             ifelse(aSum == 0, NA, aSum)}),
                       .before = 3)
bw_bl_1 <- pad_row(bw_bl_1, which(bw_bl_1$VISIT == "End of Trt.") + 1)

### Combine Tables and match output
combinedTable <- rbind(bw_stats, bw_bl_1)
names(combinedTable)[2] <- "Treatment"
names(combinedTable)[4] <- "Planned Relative Time"
combinedTable[,"Treatment"] <- apply(combinedTable, 1, function(x){
  switch(x["Treatment"],
         "Placebo" = "Placebo",
         "Xanomeline Low Dose" = "Xan.Low",
         "Xanomeline High Dose" = "Xan.High",
         NA)
})
combinedTable[,"Planned Relative Time"] <- apply(combinedTable, 1, function(x){
  switch(x["Planned Relative Time"],
         "BASELINE" = "Baseline",
         "WEEK 24" = "Week 24",
         "End of Trt." = "End of Trt.",
         NA)
})
combinedTable[!is.na(combinedTable$Mean),"Mean"] <- unlist(combinedTable[!is.na(combinedTable$Mean),"Mean"]) %>%
  aaply(.margins = 1, .fun = num_fmt, digits = 1, size = 3, int_len = 2)
combinedTable[!is.na(combinedTable$SD),"SD"] <- unlist(combinedTable[!is.na(combinedTable$SD),"SD"]) %>%
  aaply(.margins = 1, .fun = num_fmt, digits = 2, size = 4, int_len = 2)
combinedTable[!is.na(combinedTable$Median),"Median"] <- unlist(combinedTable[!is.na(combinedTable$Median),"Median"]) %>%
  aaply(.margins = 1, .fun = num_fmt, digits = 1, size = 3, int_len = 2)
combinedTable[!is.na(combinedTable$`Min.`),"Min."] <- unlist(combinedTable[!is.na(combinedTable$`Min.`),"Min."]) %>%
  aaply(.margins = 1, .fun = num_fmt, digits = 1, size = 3, int_len = 2)
combinedTable[!is.na(combinedTable$`Max.`),"Max."] <- unlist(combinedTable[!is.na(combinedTable$`Max.`),"Max."]) %>%
  aaply(.margins = 1, .fun = num_fmt, digits = 1, size = 3, int_len = 2)


ht <- combinedTable %>%
  huxtable::as_hux(add_colnames=TRUE)


huxtable::bottom_border(ht)[1, ] <- 1
huxtable::bold(ht)[1, ] <- TRUE
huxtable::align(ht)[1, ] <- 'center'
huxtable::align(ht)[,c(3, 5:10)] <- "center"
huxtable::width(ht) <- 1.5
huxtable::bottom_padding(ht) <- 0
huxtable::top_padding(ht) <- 0
huxtable::col_width(ht) <- c(0.15, 0.1, 0.05, 0.1, 0.04, 0.065, 0.065, 0.065, 0.065, 0.065)
huxtable::valign(ht)[1,] <- "bottom"





# Write into doc object and pull titles/footnotes from excel file
doc <- as_rtf_doc(ht) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-7.03') %>%
  set_font_size(10)

write_rtf(doc, file='./scripts/table_examples/outputs/14-7.03.rtf')

