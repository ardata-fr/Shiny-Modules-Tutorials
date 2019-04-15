#' @export
#' @import shiny
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinyjs enable disable disabled
#' @title load_dataUI
#' @description This function has to be set in the UI part of a shiny application
#' it adds the load data box.
#' load_data function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")
#'}
load_dataUI <- function(id) {
    ns <- NS(id)

    shinyjs::useShinyjs()
    fluidRow(
        column(12,
            selectInput(ns("SI_dataset"), label = "dataset", choices = datasets(), selected = "iris"),
            selectInput(ns("SI_var"), label = "choose variable", choices = NULL),
            shinyjs::disabled(actionBttn(inputId = ns("AB_load"), label = "(Re) load !", style = "pill", color = "primary", size = "xs"))
        )
    )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @importFrom shinyWidgets actionBttn
#' @title load_dataUI
#' @description This function has to be set in the Server part of a shiny application
#'     it adds the load data windows.
#'     load_dataUI function has to be set in the UI part.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")
#'}
load_data <- function(input, output, session) {

    # Define the ReactiveValue to return : "toReturn"
    # with slots "variable", "variable_name" & "trigger"
    toReturn    <-  reactiveValues(
                        variables = NULL,
                        variable_name = NULL,
                        trigger = 0
                    )

    # Update selectInput according to dataset
    observe({
        if (!is.null(input$SI_dataset)) {
            df <- get(input$SI_dataset)
            choices <- colnames(df)[sapply(df, is.numeric)]
            updateSelectInput(session, "SI_var", choices = choices)
        }
    })

    # Enable / Disable (Re)load button
    observe({
        if (input$SI_var != "") {
            shinyjs::enable("AB_load")
        } else {
            shinyjs::disable("AB_load")
        }
    })

    # (Re)load button
    observeEvent(input$AB_load, {
        toReturn$variable       <- get(input$SI_dataset)[,input$SI_var]
        toReturn$variable_name  <- input$SI_var
        toReturn$trigger        <- toReturn$trigger + 1
    })

    return(toReturn)
}
