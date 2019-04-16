library(shiny)
library(shinyModulesTuto)

ui <- fluidPage(
    loadModulesCSS("modules_styling.css"),

    fluidRow(
        column(12,
            h1("Data from Module to Application"),

            # module UI
            load_dataUI(id = "id1"),
            tags$hr(),

            # Print the numeric vector
            uiOutput("ui_PR_results_print")
        )
    )
)

server <- function(input, output, session) {

    # Create reactiveValues from module outputs
    # 3 slots returned :
    #   - variable (the numeric vector)
    #   - variable_name (name of the variable)
    #   - trigger (not used here)
    results <- callModule(module = load_data, id = "id1")

    # Print results$variable
    output$PR_results_print <- renderPrint({
        print(results$variable)
    })

    # Set the verbatimTextOutput inside renderUI with a title "h3"
    output$ui_PR_results_print <- renderUI({
        if (is.null(results$variable)) {
            tags$span(class = "warn", "No dataset loaded")
        } else {
            tags$div(
                h3(results$variable_name),
                verbatimTextOutput("PR_results_print")
            )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)

