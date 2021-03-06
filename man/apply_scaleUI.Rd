% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_scale.R
\name{apply_scaleUI}
\alias{apply_scaleUI}
\title{apply_scaleUI}
\usage{
apply_scaleUI(id)
}
\arguments{
\item{id}{An id that will be used to create a namespace}
}
\value{
UI page
}
\description{
This function has to be set in the UI part of a shiny application
    It add a windows containing an actionButton to apply scale on numeric vector.
    apply_scale function has to be set in the Server part.
}
\examples{
\dontrun{
# In UI :
apply_scaleUI(id = "mod4")
# In Server
data_module4   <- callModule(module = apply_scale,
                             id = "mod4",
                             variable = reactive(dataset$data_var_x))
}
}
