#' @export
#' @import shiny
#' @title data_infoUI
#' @description This function has to be set in the UI part of a shiny application
#' it adds a panel of width 4. Its title is the dataset name and its content is the
#' dataset dimensions.
#' data_info function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' data_infoUI(id = "mod1")
#' # In Server
#' callModule(module = data_info, id = "mod1")
#'}
data_infoUI <- function(id) {
    ns <- NS(id)

    column(
        width = 4,
        uiOutput(ns("result"))
    )

}

#' @export
#' @import shiny
#' @importFrom shinyWidgets panel
#' @title data_info
#' @description This function has to be set in the Server part of a shiny application.
#' load_dataUI function has to be set in the UI part.
#' @param input,output,session Not real parameter, should not be set manually. Done by callModule automatically.
#' @param data data.frame (not a reactive)
#' @param data_name Dataset name (not a reactive)
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' data_infoUI(id = "mod1")
#' # In Server
#' callModule(module = data_info, id = "mod1")
#'}
data_info <- function(input, output, session, data, data_name) {

    ns <- session$ns

    output$result <- renderUI({
        panel(
            heading = paste("Information for dataset :", data_name),
            status = "success",
            tags$strong("Dimensions :"),
            tags$br(),
            tags$pre(paste(dim(data), collapse = "/"))
        )
    })
}
