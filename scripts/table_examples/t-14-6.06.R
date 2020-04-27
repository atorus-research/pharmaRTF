## Table 14-6.06

library(plyr)
library(huxtable)
library(glue)
library(tidyverse, lib.loc = .libPaths()[2])
library(haven)
library(tibble)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

adlbhy <- read_xpt(glue("{adam_lib2}/adlbhy.xpt")) %>%
  filter(SAFFL == "Y")

adlbhy1 <- adlbhy %>%
  filter(PARAM == "Bilirubin 1.5 x ULN")

adlbhy2 <- ddply(adlbhy1, c("USUBJID"), function(x) {
  shifts <- unique(x[, "SHIFT1"])
  if(!(any(x[, "ABLFL"] %in% "Y"))) return(data.frame())
  if("Normal to High" %in% shifts) {
    data.frame(
      USUBJID = unique(x[, "USUBJID"]),
      TRTP = unique(x[, "TRTP"]),
      shft = "Normal to High"
    )
  } else if ("High to High" %in% shifts){
    data.frame(
      USUBJID = unique(x[, "USUBJID"]),
      TRTP = unique(x[, "TRTP"]),
      shft = "High to High"
    )
  }else if ("High to Normal" %in% shifts){
    data.frame(
      USUBJID = unique(x[, "USUBJID"]),
      TRTP = unique(x[, "TRTP"]),
      shft = "High to Normal"
    )
  }else {
    data.frame(
      USUBJID = unique(x[, "USUBJID"]),
      TRTP = unique(x[, "TRTP"]),
      shft = "Normal to Normal"
    )
  }
})

