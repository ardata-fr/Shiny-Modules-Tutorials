library(shiny)
library(shinyjs)
require(shinyWidgets)
library(shinyModulesTuto)

ui <- fluidPage(
    useShinyjs(),
    loadModulesCSS("modules_styling.css"),

    tags$h1("Application with dynamic module calls"),
    tags$br(),
    fluidRow(
        column(
            width = 4,
            panel(
                heading = "Module : load_data",
                status = "info",
                load_datasetUI(id = "mod1")
            )
        )
    ),
    fluidRow(
        uiOutput("all_results")
    )
)

server <- function(input, output, session) {

    # ReactiveValue that contains all the UI returned by module data_info called dynamically
    rv <- reactiveValues(all_ui = list())

    # Call module load_dataset
    data_mod1 <- callModule(module = load_dataset, id = "mod1")

    # When a new dataset is loaded, call dynamically the module 'data_info'
    # And add an element to the list rv$all_ui that is rendered through a renderUI / tagList
    observeEvent(data_mod1$trigger, {
        req(data_mod1$trigger>0)
        callModule(
            module = data_info,
            id = data_mod1$trigger,
            data = data_mod1$data,
            data_name = data_mod1$data_name
        )
        rv$all_ui[[data_mod1$trigger]] <<- data_infoUI(id = data_mod1$trigger)
    })

    output$all_results <- renderUI({
        tagList(rv$all_ui)
    })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))

