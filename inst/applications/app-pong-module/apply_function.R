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
#' # In UI :
#' apply_functionUI(id = "mod2")
#' # In Server
#' data_module2   <- callModule(module = apply_function,
#'                              id = "mod2",
#'                              variable = reactive(dataset$data_var_x),
#'                              name = "Axis X")
#'}
apply_functionUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        column(12,
            uiOutput(ns("ui_RB_funs")),
            uiOutput(ns("ui_AB_apply")),
            uiOutput(ns("ui_DIV_warn"))
        )
    )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_function
#' @description This function has to be set in the Server part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param variable Numeric. Vector containing dataset to apply function on.
#' @param name Character. Name of the variable (used only on UI part).
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' apply_functionUI(id = "mod2")
#' # In Server
#' data_module2   <- callModule(module = apply_function,
#'                              id = "mod2",
#'                              variable = reactive(dataset$data_var_x),
#'                              name = "Axis X")
#'}
apply_function <- function(input, output, session, variable = NULL, name = "") {
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

    # Radio Button
    output$ui_RB_funs <- renderUI({
        radioButtons(ns("RB_funs"), label = "Function",
            choices = c("log", "abs", "sqrt"), selected = "log")
    })

    # apply button
    output$ui_AB_apply <- renderUI({
        if (is.null(variable())) {
            shinyjs::disabled(
                actionButton(ns("AB_apply"), label = "Apply function !")
            )
        } else {
            actionButton(ns("AB_apply"), label = "Apply function !")
        }
    })

    # ReactiveValue to return
    toReturn <- reactiveValues( result = NULL,
                                trigger = NULL,
                                transformation = NULL)

    # Apply function on variable
    observeEvent(input$AB_apply, {
        if (input$RB_funs == "log") {
            toReturn$result <- log(variable())
        } else if (input$RB_funs == "abs") {
            toReturn$result <- abs(variable())
        } else if (input$RB_funs == "sqrt") {
            toReturn$result <- sqrt(variable())
        }
        toReturn$trigger        <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
        toReturn$transformation <- input$RB_funs
    })

    return(toReturn)
}
