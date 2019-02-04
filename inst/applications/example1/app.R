library(shiny)
library(shinyjs)

ui <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        loadModulesCSS("modules_styling.css")
    ),

    title = "Demo use of modules",

    sidebarLayout(
        sidebarPanel(width = 6,
            h3("Module load_data"),
            fluidRow(
                load_dataUI(id = "mod1")
            ),
            hr(),
            h3("History"),
            fluidRow(
                column(6, class = "history", uiOutput("ui_DIV_history_x")),
                column(6, class = "history", uiOutput("ui_DIV_history_y"))
            ),
            hr(),
            h3("Module apply_function"),
            fluidRow(
                column(6, fluidRow(apply_functionUI(id = "mod2"))),
                column(6, fluidRow(apply_functionUI(id = "mod3")))
            ),
            hr(),
            h3("Module apply_scale"),
            fluidRow(
                column(6, fluidRow(apply_scaleUI(id = "mod4"))),
                column(6, fluidRow(apply_scaleUI(id = "mod5")))
            )
        ),
        mainPanel(width = 6,
            h3("Application"),
            tabsetPanel(type = "tabs",
                tabPanel("X",
                    plotOutput("PL_var_x"),
                    verbatimTextOutput("PR_var_x")
                ),
                tabPanel("Y",
                    plotOutput("PL_var_y"),
                    verbatimTextOutput("PR_var_y")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    # ReactiveValue that "belongs" to Application and updated through all modules
    dataset <- reactiveValues(data_var_x = NULL, data_var_y = NULL)

    ##############################
    ## Module use 1 : Load Data ##
    ##############################
    {
        # Load dataset with module 1
        data_module1 <- callModule(module = load_data, id = "mod1")

        # When dataset loaded (trigger change) :
        #   - Update dataset X & Y according to module load_data outputs "data_var_x" & "data_var_y"
        #   - Init history for X & Y
        observeEvent(data_module1$trigger, {
            dataset$data_var_x <- data_module1$data_var_x
            dataset$data_var_y <- data_module1$data_var_y
            histo$transformations_x <- c()
            histo$transformations_y <- c()
        })
    }

    #######################
    ## Functions History ##
    #######################
    {
        # Reactive Value used to keep history of functions applied.
        histo <- reactiveValues(transformations_x = c(),
                                transformations_y = c())

        # UI output for X history
        output$ui_DIV_history_x <- renderUI({
            div(
                "Transformation(s) on X :",
                if (length(histo$transformations_x) > 0) {
                    tags$ul(HTML(sapply(histo$transformations_x, function(x) as.character(tags$li(x)))))
                }
            )
        })

        # UI output for Y history
        output$ui_DIV_history_y <- renderUI({
            div(
                "Transformation(s) on Y :",
                if (length(histo$transformations_y) > 0) {
                    tags$ul(HTML(sapply(histo$transformations_y, function(x) as.character(tags$li(x)))))
                }
            )
        })
    }

    #######################################
    ## Module use 2 & 3 : Apply Function ##
    #######################################
    {
        # Call modules apply_function for X & Y
        data_module2   <- callModule(module = apply_function, id = "mod2", variable = reactive(dataset$data_var_x), name = "Axis X")
        data_module3   <- callModule(module = apply_function, id = "mod3", variable = reactive(dataset$data_var_y), name = "Axis Y")

        # When applied function (trigger change) on X :
        #   - Update dataset X according to modules apply_function output "variable"
        #   - Update history for X according to modules apply_function output "transformation"
        observeEvent(data_module2$trigger, {
            dataset$data_var_x      <- data_module2$variable
            histo$transformations_x <- c(histo$transformations_x, data_module2$transformation)
        })

        # When applied function (trigger change) on Y :
        #   - Update dataset Y according to modules apply_function output "variable"
        #   - Update history for Y according to modules apply_function output "transformation"
        observeEvent(data_module3$trigger, {
            dataset$data_var_y      <- data_module3$variable
            histo$transformations_y <- c(histo$transformations_y, data_module3$transformation)
        })
    }

    ####################################
    ## Module use 4 & 5 : Apply Scale ##
    ####################################
    {
        # Call modules scale for X & Y
        data_module4   <- callModule(module = apply_scale, id = "mod4", variable = reactive(dataset$data_var_x), name = "Axis X")
        data_module5   <- callModule(module = apply_scale, id = "mod5", variable = reactive(dataset$data_var_y), name = "Axis Y")

        # When applied scale (trigger change) on X :
        #   - Update dataset X according to modules apply_scale output "variable"
        #   - Update history for X with "scale"
        observeEvent(data_module4$trigger, {
            dataset$data_var_x      <- data_module4$variable
            histo$transformations_x <- c(histo$transformations_x, "scale")
        })

        # When applied scale (trigger change) on Y :
        #   - Update dataset Y according to modules apply_scale output "variable"
        #   - Update history for Y  with "scale"
        observeEvent(data_module5$trigger, {
            dataset$data_var_y      <- data_module5$variable
            histo$transformations_y <- c(histo$transformations_y, "scale")
        })
    }

    ###########################
    ## Output in application ##
    ###########################
    {
        # Plot output (hist) for X
        output$PL_var_x <- renderPlot({
            req(dataset$data_var_x)
            hist(dataset$data_var_x, main = data_module1$var_x_name, xlab = NULL)
        })

        # Plot output (hist) for Y
        output$PL_var_y <- renderPlot({
            req(dataset$data_var_y)
            hist(dataset$data_var_y, main = data_module1$var_y_name, xlab = NULL)
        })

        # Print output (summary) for X
        output$PR_var_x <- renderPrint({
            req(dataset$data_var_x)
            print(summary(dataset$data_var_x))
        })

        # Print output (summary) for Y
        output$PR_var_y <- renderPrint({
            req(dataset$data_var_y)
            print(summary(dataset$data_var_y))
        })
    }
}

# Run the application
shinyApp(ui = ui, server = server)

