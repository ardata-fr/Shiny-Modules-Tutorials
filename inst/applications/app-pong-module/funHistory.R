#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_functionUI
#' @description This function has to be set in the UI part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' apply_function function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI
#' funHistoryUI(id = "id")
#'
#' # In Server
#' callModule(module = funHistory, id = "id", histo = reactive(rv$fun_historic))
#'}
funHistoryUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        column(12,
            uiOutput(ns("ui_DIV_history")),
            uiOutput(ns("ui_DIV_warn"))
        )
    )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title funHistory
#' @description This function has to be set in the Server part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param histo Numeric. Vector containing function applied.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI
#' funHistoryUI(id = "id")
#'
#' # In Server
#' callModule(module = funHistory, id = "id", histo = reactive(rv$fun_historic))
#'}
funHistory <- function(input, output, session, histo = NULL) {

    ns <- session$ns
    
    # Create tags$li from histo() parameter
    output$ui_DIV_history <- renderUI({
        if (length(histo()) > 0) {
            tags$div(
                tags$ul(HTML(sapply(histo(), function(x) as.character(tags$li(x)))))
            )
        } else {
            tags$div(
                span(class = "warn", "No function used")
            )
        }
    })
}
