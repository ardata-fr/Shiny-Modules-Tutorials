library(shiny)
library(shinyjs)
library(shinyWidgets)
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
                load_dataUI(id = "mod1")
            )
        ),
        column(
            width = 4,
            panel(
                heading = "Summary outside modules",
                status = "info",
                uiOutput("ui_summary")
            )
        )
    ),
    fluidRow(
        column(
            width = 12,
            tabsetPanel(
                id = "all_tabs"
            )
        )
    )
)

server <- function(input, output, session) {

    trick <- reactiveVal(0)

    # List that contains reactiveValues returned by module merge_modules
    res <- list()

    # Call module load_dataset
    data_mod1 <- callModule(module = load_data, id = "mod1")

    obs_close <- list()

    # Function to add a cross after tabPanel title
    tabTitle <- function(name, id) {
        tags$span(
            name, HTML("&nbsp;"),
            tags$span(
                id = paste0("close", id),
                class = "close",
                HTML("&times;")
            )
        )
    }

    # Fonction addTab to add a new tab
    addTab <- function(id, name, data) {
        # call mainInterface
        res[[paste(id)]] <<- callModule(
            module = merge_modules,
            id = id,
            data = data,
            name = name
        )

        # Add tab
        appendTab(
            inputId = "all_tabs",
            tabPanel(
                title = tabTitle(name, id),
                value = id,
                tags$br(),
                merge_modulesUI(id = id)
            ),
            select = TRUE
        )

        obs_close[[paste(id)]] <<- observe({
            shinyjs::onclick(id = paste0("close", id), closeTab(id = id))
        })

        trick(trick() + 1)
    }

    closeTab <- function(id) {
        # Remove tab from UI
        removeTab(inputId = "all_tabs", target = paste(id))
        res[[paste(id)]] <<- NULL
        trick(trick() + 1)
    }

    observeEvent(data_mod1$trigger, {
        req(data_mod1$trigger>0)

        addTab(
            id = data_mod1$trigger,
            name = data_mod1$variable_name,
            data = data_mod1$variable
        )
    })

    # Summary of all opened tabs
    output$ui_summary <- renderUI({
        fool <- trick()
        zz <- lapply(res, reactiveValuesToList)

        if (length(zz) == 0) {
            content <- div(
                class = "warn ",
                "No variable loaded"
            )
        } else {
            content <- tags$ul(lapply(zz, function(x) {
                tags$li(paste(x$name, ":", x$nb_funs))
            }))
        }

        tagList(
            div(
                class = "sum_title",
                "Variables loaded with number of functions applied :"
            ),
            content
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))

