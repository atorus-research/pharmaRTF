### Table 14-1.02 Summary of End of Study Data

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

adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))
