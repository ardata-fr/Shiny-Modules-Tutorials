#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title show_dataUI
#' @description This function has to be set in the UI part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' show_data function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' show_dataUI(id = "mod3")
#' # In Server
#' callModule(module = show_data,
#'     id = "mod3",
#'     variable = reactive(dataset$data_var_x))
#'}
show_dataUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        column(12,
            uiOutput(ns("ui_DIV_title")),
            uiOutput(ns("ui_PL_histogram_var")),
            verbatimTextOutput(ns("PR_summary_var"))
        )
    )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title show_data
#' @description This function has to be set in the Server part of a shiny application
#'     it adds the load data windows.
#'     show_dataUI function has to be set in the UI part.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param variable Reactive numeric vector
#' @param variable_name Reactive character
#' @param title Non-reactive character
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' show_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = show_data, id = "mod3", variable = variable, variable_name = variable_name)
#'}
show_data <- function(input, output, session, variable = NULL, variable_name = NULL, title = NULL) {

    ns <- session$ns

    # Histogram
    output$PL_histogram_var <- renderPlot({
        hist(variable(), main = variable_name(), xlab = NULL)
    })

    # Use a renderUI of renderPlot to print "no dataset loaded" if no data
    output$ui_PL_histogram_var <- renderUI({
        if (is.null(variable())) {
            tags$span(class = "warn", "No dataset loaded")
        } else {
            plotOutput(ns("PL_histogram_var"))
        }
    })

    # Summary
    output$PR_summary_var <- renderPrint({
        req(variable())
        print(summary(variable()))
    })

    # Title
    output$ui_DIV_title <- renderUI({
        if (!is.null(title)) h4(title)
    })
}
