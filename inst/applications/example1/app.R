library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyModulesTuto)

ui <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        loadModulesCSS("modules_styling.css")
    ),

    tags$h1("Demo use of modules"),

    tags$div(style = "display:flex",
        dropdownButton(inputId = "dd-data",
            h3("Module 1 : load_data"),
            load_dataUI(id = "mod1"),
            circle = TRUE,
            status = "primary",
            icon = icon("database"),
            width = "500px",
            tooltip = tooltipOptions(title = "Load data")
        ),
        dropdownButton(inputId = "dd-fun",
            h3("Module 2 : apply_function"),
            fluidRow(
                column(6, apply_functionUI(id = "mod2_x")),
                column(6, apply_functionUI(id = "mod2_y"))
            ),
            circle = TRUE,
            status = "success",
            icon = icon("gear"),
            width = "500px",
            tooltip = tooltipOptions(title = "Apply a function")
        ),
        dropdownButton(inputId = "dd-scale",
            h3("Module 3 : apply_scale"),
            fluidRow(
                column(6, apply_scaleUI(id = "mod3_x")),
                column(6, apply_scaleUI(id = "mod3_y"))
            ),
            circle = TRUE,
            status = "warning",
            icon = icon("balance-scale"),
            width = "500px",
            tooltip = tooltipOptions(title = "Scale data")
        )
    ),
    h3("Histogram & Summary"),
    fluidRow(
        column(6,
            h4("X vector"),
            uiOutput("ui_PL_var_x"),
            verbatimTextOutput("PR_var_x")
        ),
        column(6,
            h4("Y vector"),
            uiOutput("ui_PL_var_y"),
            verbatimTextOutput("PR_var_y")
        )
    ),
    hr(),
    h3("Module 4 : funHistory"),
    fluidRow(
        column(6, funHistoryUI(id = "mod4_x")),
        column(6, funHistoryUI(id = "mod4_y"))
    )
)

server <- function(input, output, session) {

    # ReactiveValue that "belongs" to Application and updated through all modules
    dataset <-  reactiveValues( var_x = NULL, var_y = NULL,
                                fun_applied_x = c(), fun_applied_y = c())

    ############################+
    ## Module 1 : Load Data  ####
    ##     id call = "mod1"  ###+
    ############################+
    {
        # Load dataset with module 1
        data_mod1 <- callModule(module = load_data, id = "mod1")

        # When dataset loaded (trigger change) :
        #   - Update dataset X & Y according to module load_data outputs "data_var_x" & "data_var_y"
        #   - Init history of functions applied for X & Y
        observeEvent(data_mod1$trigger, {
            req(data_mod1$trigger>0)
            dataset$var_x         <- data_mod1$var_x
            dataset$var_y         <- data_mod1$var_y
            histo$fun_applied_x   <- c()
            histo$fun_applied_y   <- c()
            toggleDropdownButton("dd-data")
        })
    }

    #########################################+
    ## Module 2 : Apply Function          ####
    ##     id call = "mod2_x" & "mod2_y"  ###+
    #########################################+
    {
        # Call modules apply_function for X & Y
        data_mod2_x   <- callModule(module = apply_function, id = "mod2_x", variable = reactive(dataset$var_x), name = "X vector")
        data_mod2_y   <- callModule(module = apply_function, id = "mod2_y", variable = reactive(dataset$var_y), name = "Y vector")

        # When applied function (trigger change) on X :
        #   - Update dataset X according to modules apply_function output "result"
        #   - Update history of functions applied for X according to modules apply_function output "transformation"
        observeEvent(data_mod2_x$trigger, {
            dataset$var_x       <- data_mod2_x$result
            histo$fun_applied_x <- c(histo$fun_applied_x, data_mod2_x$transformation)
        })

        # When applied function (trigger change) on Y :
        #   - Update dataset Y according to modules apply_function output "result"
        #   - Update history of functions applied for Y according to modules apply_function output "transformation"
        observeEvent(data_mod2_y$trigger, {
            dataset$var_y       <- data_mod2_y$result
            histo$fun_applied_y <- c(histo$fun_applied_y, data_mod2_y$transformation)
        })
    }

    #########################################+
    ## Module 3 : Apply Scale             ####
    ##     id call = "mod3_x" & "mod3_y"  ###+
    #########################################+
    {
        # Call modules scale for X & Y
        data_mod3_x   <- callModule(module = apply_scale, id = "mod3_x", variable = reactive(dataset$var_x), name = "X vector")
        data_mod3_y   <- callModule(module = apply_scale, id = "mod3_y", variable = reactive(dataset$var_y), name = "Y vector")

        # When applied scale (trigger change) on X :
        #   - Update dataset X according to modules apply_scale output "result"
        #   - Update history of functions applied for X with "scale"
        observeEvent(data_mod3_x$trigger, {
            dataset$var_x       <- data_mod3_x$result
            histo$fun_applied_x <- c(histo$fun_applied_x, "scale")
        })

        # When applied scale (trigger change) on Y :
        #   - Update dataset Y according to modules apply_scale output "result"
        #   - Update history of functions applied for Y  with "scale"
        observeEvent(data_mod3_y$trigger, {
            dataset$var_y       <- data_mod3_y$result
            histo$fun_applied_y <- c(histo$fun_applied_y, "scale")
        })
    }

    #########################################+
    ## Module 4 : Functions History       ####
    ##     id call = "mod4_x" & "mod4_y"  ###+
    #########################################+
    {
        # Reactive Value used to keep history of functions applied.
        histo <- reactiveValues(fun_applied_x = c(),
                                fun_applied_y = c())

        callModule(module = funHistory, id = "mod4_x", histo = reactive(histo$fun_applied_x), name = "X vector")
        callModule(module = funHistory, id = "mod4_y", histo = reactive(histo$fun_applied_y), name = "Y vector")
    }

    ############################+
    ## Output in application ####
    ############################+
    {
        # Plot output (hist) for X
        output$PL_var_x <- renderPlot({
            # req(dataset$var_x)
            hist(dataset$var_x, main = data_mod1$var_x_name, xlab = NULL)
        })

        # Use a renderUI of renderPlot to let empty space if no plot
        output$ui_PL_var_x <- renderUI({
            if (is.null(dataset$var_x)) return(NULL)
            plotOutput("PL_var_x")
        })

        # Plot output (hist) for Y
        output$PL_var_y <- renderPlot({
            # req(dataset$var_y)
            hist(dataset$var_y, main = data_mod1$var_y_name, xlab = NULL)
        })

        # Use a renderUI of renderPlot to let empty space if no plot
        output$ui_PL_var_y <- renderUI({
            if (is.null(dataset$var_y)) return(NULL)
            plotOutput("PL_var_y")
        })

        # Print output (summary) for X
        output$PR_var_x <- renderPrint({
            req(dataset$var_x)
            print(summary(dataset$var_x))
        })

        # Print output (summary) for Y
        output$PR_var_y <- renderPrint({
            req(dataset$var_y)
            print(summary(dataset$var_y))
        })
    }
}

# Run the application
shinyApp(ui = ui, server = server)

