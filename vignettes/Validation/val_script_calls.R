#Validation Script

# Source all of the files in the rtf_test_files directory
lapply(list.files("vignettes/Validation/Test_Case_Code", full.names = TRUE,
                  pattern = ".R$"), source)

shiny::runApp("vignettes/Validation/app.R")
