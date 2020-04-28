#Validation Script

library(pharmaRTF)
library(tidyverse, lib.loc = .libPaths()[2])
library(testthat)
source("~/pharmaRTF/vignettes/helper_test_code.R")

make_test_case_rmd("~/pharmaRTF/vignettes/test_cases.csv")
make_specification_rmd("~/pharmaRTF/vignettes/specs.csv")

# Source all of the files in the rtf_test_files directory
lapply(list.files("~/pharmaRTF/vignettes/Validation/Test_Case_Code", full.names = TRUE,
                  pattern = ".R$"), source)

shiny::runApp("~/pharmaRTF/vignettes/Validation/app.R")
