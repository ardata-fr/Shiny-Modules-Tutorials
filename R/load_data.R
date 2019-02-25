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
    
    column(12,
        fluidRow(
            column(12,
                div(class = "show_id", paste("call id : ", id))
            )
        ),
        fluidRow(
            column(12,
                selectInput(ns("SI_dataset"), label = "dataset", choices = datasets(), selected = "iris")
            )
        ),
        fluidRow(
            column(6,
                selectInput(ns("SI_var_x"), label = "X vector", choices = NULL)
            ),
            column(6,
                selectInput(ns("SI_var_y"), label = "Y vector", choices = NULL)
            )
        ),
        fluidRow(
            column(12,
                shinyjs::disabled(actionBttn(inputId = ns("AB_load"), label = "(Re) load !", style = "pill", color = "primary", size = "xs"))
            )
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
    # with slots "var_x" & "var_y"
    toReturn    <-  reactiveValues(
                        var_x = NULL,
                        var_y = NULL,
                        trigger = 0
                    )

    # Update selectInput according to dataset
    observe({
        if (!is.null(input$SI_dataset)) {
            df <- get(input$SI_dataset)
            choices <- colnames(df)[sapply(df, is.numeric)]
            updateSelectInput(session, "SI_var_x", choices = choices)
            updateSelectInput(session, "SI_var_y", choices = choices)
        }
    })

    # Enable / Disable (Re)load button
    observe({
        if (input$SI_var_y != "" & input$SI_var_x != "") {
            shinyjs::enable("AB_load")
        } else {
            shinyjs::disable("AB_load")
        }
    })

    # (Re)load button
    observeEvent(input$AB_load, {
        toReturn$var_x     <- get(input$SI_dataset)[,input$SI_var_x]
        toReturn$var_y     <- get(input$SI_dataset)[,input$SI_var_y]
        toReturn$var_x_name     <- input$SI_var_x
        toReturn$var_y_name     <- input$SI_var_y
        toReturn$trigger        <- toReturn$trigger + 1
    })

    return(toReturn)
}
