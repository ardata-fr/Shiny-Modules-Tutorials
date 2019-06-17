#' @export
#' @import shiny
#' @title merge_modulesUI
#' @description This function has to be set in the UI part of a shiny application.
#'     The module merges modules apply_function, apply_scale, show_data & funHistory
#'     Function merge_modules must be set in server part.
#' @param id namespace identifier for the module
#' @return div
#' @examples
#' \dontrun{
#' # In UI :
#' merge_modulesUI(id = "id1")
#' # In Server
#' data_module1 <- callModule(
#'   module = merge_modules, 
#'   id = "id1", 
#'   data = iris$Sepal.Length, 
#'   name = "Sepal.Length")
#'}
merge_modulesUI <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            column(
                width = 4,
                panel(
                    heading = "Module : apply_function",
                    status = "warning",
                    apply_functionUI(id = ns("mod2"))
                )
            ),
            column(
                width = 4,
                panel(
                    heading = "Module : apply_scale",
                    status = "danger",
                    apply_scaleUI(id = ns("mod3"))
                )
            )
        ),
        fluidRow(
            column(
                width = 4,
                panel(
                    heading = "Module : show_data",
                    status = "success",
                    show_dataUI(id = ns("mod4"))
                )
            ),
            column(
                width = 4,
                panel(
                    heading = "Module : funHistory",
                    status = "default",
                    funHistoryUI(id = ns("mod5"))
                )
            )
        )
    )
}

#' @export
#' @import shiny
#' @title merge_modules
#' @description This function has to be set in the Server part of a shiny application.
#'     The module merges modules apply_function, apply_scale, show_data & funHistory
#'     merge_modulesUI function has to be set in the UI part.
#' @param input,output,session mandatory arguments for modules to be valid. These
#' should not to be defined as they will be handled by shiny.
#' @param data Numeric vector (not a reactive variable).
#' @param name Variable name (not a reactive variable).
#' @return reactiveValues with 2 slots :
#' name : name of the variable.
#' nb_funs : number of functions applied on vector.
#' @examples
#' \dontrun{
#' # In UI :
#' merge_modulesUI(id = "id1")
#' # In Server
#' data_module1 <- callModule(
#'   module = merge_modules, 
#'   id = "id1", 
#'   data = iris$Sepal.Length, 
#'   name = "Sepal.Length")
#'}
merge_modules <- function(input, output, session, data, name) {

    ns <- session$ns

    # ReactiveValue that "belongs" to Application and updated through all modules
    rv <- reactiveValues(variable = data, fun_history = NULL)


    #################################+
    ## Module 2 : Apply Function  ####
    ##     id call = "mod2"       ###+
    #################################+
    {
        # Call module apply_function
        data_mod2 <-    callModule(
                            module = apply_function, id = "mod2",
                            variable = reactive(rv$variable)
                        )

        # When applied function (data_mod2$trigger change) :
        #   - Update rv$variable with module output "variable"
        #   - Update rv$fun_history with module output "fun"
        observeEvent(data_mod2$trigger, {
            req(data_mod2$trigger>0)
            rv$variable    <- data_mod2$result
            rv$fun_history <- c(rv$fun_history, data_mod2$fun)
        })
    }

    ##############################+
    ## Module 3 : Apply Scale  ####
    ##     id call = "mod3"    ###+
    ##############################+
    {
        # Call module scale
        data_mod3 <-    callModule(
                            module = apply_scale, id = "mod3",
                            variable = reactive(rv$variable)
                        )

        # When applied function (data_mod3$trigger change) :
        #   - Update rv$variable with module output "variable"
        #   - Update rv$fun_history with "scale"
        observeEvent(data_mod3$trigger, {
            req(data_mod3$trigger>0)
            rv$variable    <- data_mod3$result
            rv$fun_history <- c(rv$fun_history, "scale")
        })
    }

    ############################+
    ## Module 4 : Show data  ####
    ##     id call = "mod4"  ###+
    ############################+
    {
        # Call module show_data
        callModule(
            module = show_data, id = "mod4",
            variable = reactive(rv$variable),
            variable_name = reactive(name),
            useggplot = TRUE
        )
    }

    ####################################+
    ## Module 5 : Functions History  ####
    ##     id call = "mod5"          ###+
    ####################################+
    {
        # Call module funHistory
        callModule(
            module = funHistory, 
            id = "mod5", 
            histo = reactive(rv$fun_history)
        )
    }
    
    #############+
    ## Return ####
    #############+
    {
        # The module return a reactiveValues with 2 slots :
        #   - number of functions applied
        toReturn <- reactiveValues(name = NULL, nb_funs = 0)
        
        observe({
            toReturn$name <- name
            toReturn$nb_funs <- length(rv$fun_history)
        })
        
        return(toReturn)
    }
}
