library(shiny)
library(shinyjs)
require(shinyWidgets)
library(shinyModulesTuto)

ui <- fluidPage(
    useShinyjs(),
    loadModulesCSS("modules_styling.css"),

    tags$h1("Application with multiple modules using the same reactive"),
    tags$br(),

    fluidRow(
        column(4,
            panel(
                heading = "Module : load_data",
                status = "info",
                load_dataUI(id = "mod1")
            )
        ),
        column(4,
            panel(
                heading = "Module : apply_function",
                status = "warning",
                apply_functionUI(id = "mod2")
            )
        ),
        column(4,
            panel(
                heading = "Module : apply_scale",
                status = "danger",
                apply_scaleUI(id = "mod3")
            )
        )
    ),
    fluidRow(
        column(6,
            panel(
                heading = "Module : show_data",
                status = "success",
                show_dataUI(id = "mod4")
            )
        ),
        column(6,
            panel(
                heading = "Module : funHistory",
                status = "default",
                funHistoryUI(id = "mod5")
            )
        )
    )
)

server <- function(input, output, session) {

    # ReactiveValue that "belongs" to Application and updated through all modules
    rv <- reactiveValues(variable = NULL, fun_history = NULL)

    ############################+
    ## Module 1 : Load Data  ####
    ##     id call = "mod1"  ###+
    ############################+
    {
        # Call module load_data
        data_mod1 <- callModule(module = load_data, id = "mod1")

        # When dataset loaded (data$mod1$trigger change) :
        #   - Update rv$variable with module output "variable"
        #   - (Re)Init history of functions rv$fun_history
        observeEvent(data_mod1$trigger, {
            req(data_mod1$trigger>0)
            rv$variable    <- data_mod1$variable
            rv$fun_history <- c()
        })
    }

    #################################+
    ## Module 2 : Apply Function  ####
    ##     id call = "mod2"       ###+
    #################################+
    {
        # Call module apply_function
        data_mod2 <-    callModule(module = apply_function, id = "mod2",
                            variable = reactive(rv$variable))

        # When applied function (data_mod2$trigger change) :
        #   - Update rv$variable with module output "variable"
        #   - Update rv$fun_history with module output "fun"
        observeEvent(data_mod2$trigger, {
            req(data_mod2$trigger>0)
            rv$variable    <- data_mod2$result
            rv$fun_history <- c(rv$fun_history, data_mod2$fun)
        })
    }

    ##############################+
    ## Module 3 : Apply Scale  ####
    ##     id call = "mod3"    ###+
    ##############################+
    {
        # Call module scale
        data_mod3 <-    callModule(module = apply_scale, id = "mod3",
                            variable = reactive(rv$variable))

        # When applied function (data_mod3$trigger change) :
        #   - Update rv$variable with module output "variable"
        #   - Update rv$fun_history with "scale"
        observeEvent(data_mod3$trigger, {
            req(data_mod3$trigger>0)
            rv$variable    <- data_mod3$result
            rv$fun_history <- c(rv$fun_history, "scale")
        })
    }

    ############################+
    ## Module 4 : Show data  ####
    ##     id call = "mod4"  ###+
    ############################+
    {
        # Call module show_data
        callModule(module = show_data, id = "mod4",
            variable = reactive(rv$variable),
            variable_name = reactive(data_mod1$variable_name),
            useggplot = TRUE)
    }

    ####################################+
    ## Module 5 : Functions History  ####
    ##     id call = "mod5"          ###+
    ####################################+
    {
        # Call module funHistory
        callModule(module = funHistory, id = "mod5", histo = reactive(rv$fun_history))
    }
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))

