#import datasets
datasets <- function() {
    tmp <-  unlist(
        sapply(ls("package:datasets"), function(x){
            "data.frame" %in% class(get(x))
        })
    )
    names(tmp[tmp])
}

#' @export
#' @importFrom shiny addResourcePath tagList
#' @importFrom tools file_ext
#' @title loadModulesCSS
#' @description This function has to be set in the ui part to link CSS file.
#' @param cssFilenames Character css file to include (extension mandatory) Default c() means all.
#' @return Load css
#' @examples
#' \dontrun{
#' # In UI
#' shinyModulesTuto::loadModulesCSS()
#' }
loadModulesCSS <- function(cssFilenames = c()) {
    if (length(cssFilenames) == 0) {
        cssFilenames <- list.files(path = system.file(package = "shinyModulesTuto", "www"), pattern = "*[.]css")
    }

    # Add ressources
    eval.parent(addResourcePath('www', system.file(package="shinyModulesTuto", "www")))

    links <- list()
    for (f in cssFilenames) {
        if (file_ext(f) == "css") {
            links[[f]] <- tags$link(rel = "stylesheet", type = "text/css", href = file.path("www", f))
        }
    }
    return(do.call(tagList, links))
}
