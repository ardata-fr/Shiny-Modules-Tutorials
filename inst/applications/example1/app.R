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
                column(6, fluidRow(apply_functionUI(id = "mod2_x"))),
                column(6, fluidRow(apply_functionUI(id = "mod2_y")))
            ),
            hr(),
            h3("Module apply_scale"),
            fluidRow(
                column(6, fluidRow(apply_scaleUI(id = "mod3_x"))),
                column(6, fluidRow(apply_scaleUI(id = "mod3_y")))
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
    dataset <- reactiveValues(var_x = NULL, var_y = NULL)

    #############################
    ## Module 1 : Load Data    ##
    ##     id call = "mod1"    ##
    #############################
    {
        # Load dataset with module 1
        data_mod1 <- callModule(module = load_data, id = "mod1")

        # When dataset loaded (trigger change) :
        #   - Update dataset X & Y according to module load_data outputs "data_var_x" & "data_var_y"
        #   - Init history for X & Y
        observeEvent(data_mod1$trigger, {
            dataset$var_x           <- data_mod1$var_x
            dataset$var_y           <- data_mod1$var_y
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

    ########################################
    ## Module 2 : Apply Function          ##
    ##     id call = "mod2_x" & "mod2_y"  ##
    ########################################
    {
        # Call modules apply_function for X & Y
        data_mod2_x   <- callModule(module = apply_function, id = "mod2_x", variable = reactive(dataset$var_x), name = "Axis X")
        data_mod2_y   <- callModule(module = apply_function, id = "mod2_y", variable = reactive(dataset$var_y), name = "Axis Y")

        # When applied function (trigger change) on X :
        #   - Update dataset X according to modules apply_function output "result"
        #   - Update history for X according to modules apply_function output "transformation"
        observeEvent(data_mod2_x$trigger, {
            dataset$var_x           <- data_mod2_x$result
            histo$transformations_x <- c(histo$transformations_x, data_mod2_x$transformation)
        })

        # When applied function (trigger change) on Y :
        #   - Update dataset Y according to modules apply_function output "result"
        #   - Update history for Y according to modules apply_function output "transformation"
        observeEvent(data_mod2_y$trigger, {
            dataset$var_y           <- data_mod2_y$result
            histo$transformations_y <- c(histo$transformations_y, data_mod2_y$transformation)
        })
    }

    ########################################
    ## Module 3 : Apply Scale             ##
    ##     id call = "mod3_x" & "mod3_y"  ##
    ########################################
    {
        # Call modules scale for X & Y
        data_mod3_x   <- callModule(module = apply_scale, id = "mod3_x", variable = reactive(dataset$var_x), name = "Axis X")
        data_mod3_y   <- callModule(module = apply_scale, id = "mod3_y", variable = reactive(dataset$var_y), name = "Axis Y")

        # When applied scale (trigger change) on X :
        #   - Update dataset X according to modules apply_scale output "result"
        #   - Update history for X with "scale"
        observeEvent(data_mod3_x$trigger, {
            dataset$var_x           <- data_mod3_x$result
            histo$transformations_x <- c(histo$transformations_x, "scale")
        })

        # When applied scale (trigger change) on Y :
        #   - Update dataset Y according to modules apply_scale output "result"
        #   - Update history for Y  with "scale"
        observeEvent(data_mod3_y$trigger, {
            dataset$var_y           <- data_mod3_y$result
            histo$transformations_y <- c(histo$transformations_y, "scale")
        })
    }

    ###########################
    ## Output in application ##
    ###########################
    {
        # Plot output (hist) for X
        output$PL_var_x <- renderPlot({
            req(dataset$var_x)
            hist(dataset$var_x, main = data_mod1$var_x_name, xlab = NULL)
        })

        # Plot output (hist) for Y
        output$PL_var_y <- renderPlot({
            req(dataset$var_y)
            hist(dataset$var_y, main = data_mod1$var_y_name, xlab = NULL)
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

