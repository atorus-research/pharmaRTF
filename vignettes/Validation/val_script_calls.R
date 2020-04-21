#Validation Script

setwd("~/pharmaRTF")
library(pharmaRTF)
library(tidyverse)
library(testthat)
source("~/pharmaRTF/vignettes/helper_test_code.R")

make_test_case_rmd("~/pharmaRTF/vignettes/test_cases.csv")

# Source all of the files in the rtf_test_files directory
lapply(list.files("~/pharmaRTF/vignettes/Validation/Test_Case_Code", full.names = TRUE,
                  pattern = ".R$"), source)

shiny::runApp("~/pharmaRTF/vignettes/Validation/app.R")
