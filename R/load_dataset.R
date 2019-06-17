#' @export
#' @import shiny
#' @title load_datasetUI
#' @description This function has to be set in the UI part of a shiny application
#' it adds the load data box.
#' load_dataset function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' load_datasetUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_dataset, id = "mod1")
#'}
load_datasetUI <- function(id) {
    ns <- NS(id)

    shinyjs::useShinyjs()
    fluidRow(
        column(12,
            selectInput(ns("SI_dataset"), label = "Dataset", choices = datasets(), selected = "iris"),
            actionButton(ns("AB_load"), label = "(Re) load !")
        )
    )
}

#' @export
#' @import shiny
#' @title load_datasetUI
#' @description This function has to be set in the Server part of a shiny application
#'     it adds the load data windows.
#'     load_datasetUI function has to be set in the UI part.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' load_datasetUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_dataset, id = "mod1")
#'}
load_dataset <- function(input, output, session) {

    ns <- session$ns

    # Define the ReactiveValue to return : "toReturn"
    # with slots "data", "data_name" & "trigger"
    toReturn    <-  reactiveValues(
                        data = NULL,
                        data_name = NULL,
                        trigger = 0
                    )


    # (Re)load button
    observeEvent(input$AB_load, {
        toReturn$data       <- get(input$SI_dataset)
        toReturn$data_name  <- input$SI_dataset
        toReturn$trigger        <- toReturn$trigger + 1
    })

    return(toReturn)
}
