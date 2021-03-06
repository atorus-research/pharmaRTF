library(shiny)
library(shinydashboard)
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Validation User Application"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
            box(
                uiOutput("vurOutput"),
                actionButton("sendButton", "Submit"),
                actionButton("backButton", "Prev Question"),
                actionButton("nextButton", "Next Question"),
                actionButton("openButton" , "Open File"),
                verbatimTextOutput("checkInfo"),
                title = "Current Check"
            ),
            box(
                tableOutput("UserDf"),
                actionButton("saveButton", "Create Validation Document"),
                verbatimTextOutput("userInfo"),
                title = "User Tests"
            )
        )
    )
)

server <- function(input, output) {

    vur <- reactiveValues(df = {
        df <- read.csv("../test_cases.csv")
        df <- df[df$CheckType == "visual",]
        df$Response <- FALSE
        df$Log <- NA
        df$ID <- paste0(df$TestID, ".", df$CheckID)
        df
        })

    observeEvent(input$sendButton, {
        vur$df[((input$nextButton - input$backButton + input$sendButton - 1) %% nrow(vur$df)) + 1,
               "Response"] <- as.logical(input$vurButtons)
        vur$df[((input$nextButton - input$backButton + input$sendButton - 1) %% nrow(vur$df)) + 1,
               "Log"] <- paste0(Sys.getenv("LOGNAME"), ":", Sys.time())
    })

    output$vurOutput <- renderUI({
        radioButtons("vurButtons",
                     vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1, "Text"],
                     choices = c(TRUE, FALSE))
    })

    output$checkInfo <- renderText({
        paste0(
            "Current Test: ", vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1,
                                     "TestID"], "\n",
            "Current Check: " , vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1,
                                       "CheckID"], "\n",
            "Manual Check of: ", vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1,
                                          "OutputFile"]
        )
    })

    observeEvent(input$saveButton, {
        saveRDS(vur$df, "~/pharmaRTF/vignettes/Validation/vur_auto.Rds")
        showModal(modalDialog("Validation User Responses written"))
        rmarkdown::render("~/pharmaRTF/vignettes/Validate.Rmd", "pdf_document")
        # file.remove("~/pharmaRTF/vignettes/Validation/vur_auto.Rds")
    })

    output$UserDf <- renderTable(vur$df[, c("ID", "Text", "OutputFile", "Response")])

    output$userInfo <- renderText({
        paste0("User: ", Sys.getenv("USER"), "\n",
               "Tests Passed/Failed/Total: ",
               sum(vur$df$Response), "/",
               sum(!vur$df$Response), "/",
               length(vur$df$Response))
    })

    observeEvent(input$openButton, {
        filePath <- paste0(getwd(),
                           "/Test_Case_Code/rtf-test-files/",
                           vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1,
                                  "OutputFile"]
        )
        if(file.exists(filePath)){
            showModal(modalDialog("This doesn't work yet"))
            # system(filePath)
        } else {
            showModal(modalDialog("RTF File Not found"))
        }
    })
}

shinyApp(ui = ui, server = server)
