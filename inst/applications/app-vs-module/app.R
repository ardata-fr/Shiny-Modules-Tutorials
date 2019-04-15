library(shiny)
library(shinyModulesTuto)

ui <- fluidPage(
    loadModulesCSS("modules_styling.css"),

    fluidRow(
        column(12,
            h1("Data from Application to Module"),
            selectInput("SI_colname", label = "Choose iris numeric variable",
                choices = colnames(iris)[which(sapply(iris, is.numeric))]),

            h2("Solution 1 : Using a reactive"),
            show_dataUI(id = "id1"),
            tags$hr(),

            h2("Solution 2 : Using a reactiveValues"),
            show_dataUI(id = "id2")
        )
    )
)

server <- function(input, output, session) {

    ####################################+
    ## Solution 1 : Using a reactive ####
    ####################################+
    {
        # Reactive containing numeric vector
        variable <- reactive({
            iris[, input$SI_colname]
        })

        # Get histogram and summary with module show_data
        callModule(module = show_data, id = "id1",
                   variable = variable,
                   variable_name = reactive(input$SI_colname))
    }

    ##########################################+
    ## Solution 2 : Using a reactiveValues ####
    ##########################################+
    {
        # Create & update reactiveValues
        rv <- reactiveValues(variable = NULL)

        observe({
            rv$variable <- iris[, input$SI_colname]
        })

        callModule(module = show_data, id = "id2",
                   variable = reactive(rv$variable),
                   variable_name = reactive(input$SI_colname))
    }

}

# Run the application
shinyApp(ui = ui, server = server)

