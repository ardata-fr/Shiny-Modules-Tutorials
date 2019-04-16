#' @export
#' @title List Shiny application
#' @description List all shiny applications present in shinyModulesTuto/applications/
#' @examples
#' \dontrun{
#' listEx()
#'}
listEx <- function() {
    list.files(system.file("applications", package = "shinyModulesTuto"))
}

#' @export
#' @title Run Shiny application
#' @description Launch shiny applications present in shinyModulesTuto/applications/
#' @importFrom shiny runApp
#' @param app Character Application folder name
#' @param showcase Logical if app should be launched in showcase mode
#' @examples
#' \dontrun{
#' runEx(app = "example1")
#'}
runEx <- function(app, showcase = TRUE) {
    listApp <- list.files(system.file("applications", package = "shinyModulesTuto"))
    if (!app %in% listApp) {
        stop(paste("Valid examples are:", paste(listApp, collapse = ", ")))
    } else {
        appDir <- file.path(system.file("applications", package = "shinyModulesTuto"), app)
    }
    if (showcase) {
        runApp(appDir, display.mode = "showcase")
    } else {
        runApp(appDir, display.mode = "normal")
    }
}
