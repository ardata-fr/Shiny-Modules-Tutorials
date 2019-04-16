library(shiny)
library(shinyModulesTuto)

ui <- fluidPage(
    loadModulesCSS("modules_styling.css"),

    fluidRow(
        column(12,
            h1("Update data in Application through Module"),
            selectInput("SI_colname", label = "Choose iris numeric variable",
                choices = colnames(iris)[which(sapply(iris, is.numeric))]),

            fluidRow(
                column(3,
                    # Apply a function on selected vector
                    apply_functionUI(id = "id1")
                ),
                column(3,
                    # Show historic of applied function
                    funHistoryUI(id = "id2")
                )
            ),
            tags$hr(),

            # Print the vector
            uiOutput("ui_PR_reults_print")
        )
    )
)

server <- function(input, output, session) {

    rv <- reactiveValues(variable = NULL, fun_historic = NULL)

    # (Re)set rv$variable when user change iris colname
    observe({
        rv$variable <- iris[, input$SI_colname]
        rv$fun_historic <- NULL
    })

    # Module apply_function return reactiveValues with 3 slots :
    #   - result contains the numeric vector updated by function
    #   - fun contains the name of the function applied
    #   - trigger increases every time user apply a function
    modified_data <-    callModule(module = apply_function, id = "id1",
                            variable = reactive(rv$variable))

    # When applied function (trigger change):
    #   - Replace rv$variable by modified_data$result
    #   - Add modified_data$fun in rv$fun_historic
    observeEvent(modified_data$trigger, {
        rv$variable     <- modified_data$result
        rv$fun_historic <- c(rv$fun_historic, modified_data$fun)
    })

    # Show historic of function (rv$fun_historic) with module funHistory
    callModule(module = funHistory, id = "id2", histo = reactive(rv$fun_historic))

    # Print rv$variable
    output$PR_results_print <- renderPrint({
        print(rv$variable)
    })

    # Set the verbatimTextOutput inside renderUI with a title "h3"
    output$ui_PR_reults_print <- renderUI({
        if (is.null(rv$variable)) {
            tags$span(class = "warn", "No dataset loaded")
        } else {
            tags$div(
                h3(input$SI_colname),
                verbatimTextOutput("PR_results_print")
            )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)

