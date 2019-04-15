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
#' callModule(module = funHistory, id = "id", histo = reactive(histo$fun_applied_x), name = "X vector")
#'}
funHistoryUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        column(12,
            uiOutput(ns("ui_DIV_history"))
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
#' @param name Character. Name of the variable (used only on UI part).
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI
#' funHistoryUI(id = "id")
#'
#' # In Server
#' callModule(module = funHistory, id = "id", histo = reactive(histo$fun_applied_x), name = "X vector")
#'}
funHistory <- function(input, output, session, histo = NULL, name = "Function historic") {
    ns <- session$ns

    output$ui_DIV_history <- renderUI({
        tags$div(
            h4(name),
            if (length(histo()) > 0) {
                tags$ul(HTML(sapply(histo(), function(x) as.character(tags$li(x)))))
            }
        )
    })
}
