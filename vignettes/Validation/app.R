library(shiny)
library(shinydashboard)

Validation_User_Responses <- data.frame(
    Case_Index = c(
        "2.1.1.1",
        "2.1.1.2",
        "2.1.1.3",
        "2.1.2.1",
        "2.1.2.2",
        "2.1.2.3"
        ),
    Questions = c(
        "Verify margins are set to expected default:1,1,1,1",
        "Check attribute to verify margins can be changed to:2,.5,1.5,.25",
        "Check output to verify margins have been changed to:2,.5,1.5,.25",
        "header_footer1 default to 0.5, 0.5",
        "header_footer2 set to 0.25, 1",
        "Output changed to 0.25, 1"
        ),
    Response = FALSE,
    AutoFlag = c(
        TRUE,
        TRUE,
        FALSE,
        TRUE,
        TRUE,
        FALSE
    ),
    Log = NA
)
# saveRDS(Validation_User_Responses, file = "vignettes/vur.Rds")


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
                title = "Current Test"
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
        df = readRDS("../vur.Rds")
        df[!df$AutoFlag,]
        })

    observeEvent(input$sendButton, {
        vur$df[((input$nextButton - input$backButton + input$sendButton - 1) %% nrow(vur$df)) + 1,
               "Response"] <- as.logical(input$vurButtons)
        vur$df[((input$nextButton - input$backButton + input$sendButton - 1) %% nrow(vur$df)) + 1,
               "Log"] <- paste0(Sys.getenv("LOGNAME"), ":", Sys.time())
    })

    output$vurOutput <- renderUI({
        radioButtons("vurButtons",
                     vur$df[((input$nextButton - input$backButton + input$sendButton) %% nrow(vur$df)) + 1, "Questions"],
                     choices = c(TRUE, FALSE))
    })

    observeEvent(input$saveButton, {
        saveRDS(vur$df, "vur_auto.Rds")
        showModal(modalDialog("Validation User Responses written"))
        rmarkdown::render("../Validate.Rmd", "pdf_document")
    })

    output$UserDf <- renderTable(print(vur$df[!vur$df$AutoFlag,]))

    output$userInfo <- renderText({
        paste0("User: ", Sys.getenv("USER"), "\n",
        "Tests Passed/Failed/Total: ",
               sum(vur$df$Response), "/",
               sum(!vur$df$Response), "/",
               length(vur$df$Response))
    })
}

shinyApp(ui = ui, server = server)
