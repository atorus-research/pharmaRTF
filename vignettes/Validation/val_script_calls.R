#Validation Script

# Source all of the files in the rtf_test_files directory
lapply(list.files("vignettes/Validation/Test_Case_Code/rtf_test_files", full.names = TRUE,
                  pattern = ".R$"), source)

# Currently done manually
# source("vignettes/Validation/write_vur.R")

shiny::runApp("vignettes/Validation/app.R")
