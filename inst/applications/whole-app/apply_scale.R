#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_scaleUI
#' @description This function has to be set in the UI part of a shiny application
#'     It add a windows containing an actionButton to apply scale on numeric vector.
#'     apply_scale function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' apply_scaleUI(id = "mod4")
#' # In Server
#' data_module4   <- callModule(module = apply_scale,
#'                              id = "mod4",
#'                              variable = reactive(dataset$data_var_x))
#'}
apply_scaleUI <- function(id) {
    ns <- NS(id)
    
    fluidRow(
        column(12,
            uiOutput(ns("ui_AB_scale")),
            uiOutput(ns("ui_DIV_warn"))
        )
    )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_scaleUI
#' @description This function has to be set in the UI part of a shiny application
#'     It add a windows containing an actionButton to apply scale on numeric vector.
#'     apply_scale function has to be set in the Server part.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param variable Numeric. Vector containing dataset to apply function on.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' apply_scaleUI(id = "mod4")
#' # In Server
#' data_module4   <- callModule(module = apply_scale, id = "mod4",
#'                       variable = reactive(rv$variable))
#'}
apply_scale <- function(input, output, session, variable = NULL) {

    ns <- session$ns
    
    # Warning if no data loaded
    output$ui_DIV_warn <- renderUI({
        if (is.null(variable())) {
            div(
                tags$br(),
                span(class = "warn", "No dataset loaded")
            )
        }
    })
    
    # Action button with custom label
    output$ui_AB_scale <- renderUI({
        if (!is.null(variable())) {
            actionButton(ns("AB_scale"), label = "Scale data")
        } else {
            shinyjs::disabled(
                actionButton(ns("AB_scale"), label = "Scale data")
            )
        }
    })
    
    # ReactiveValue to return
    toReturn <- reactiveValues(result = NULL, trigger = NULL)
    
    # Apply function on variable
    observeEvent(input$AB_scale, {
        toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
        toReturn$result  <- scale(variable())[,1]
    })
    
    return(toReturn)
}