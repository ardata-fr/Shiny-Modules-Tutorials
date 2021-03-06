% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_data.R
\name{show_dataUI}
\alias{show_dataUI}
\title{show_dataUI}
\usage{
show_dataUI(id)
}
\arguments{
\item{id}{An id that will be used to create a namespace}
}
\value{
UI page
}
\description{
This function has to be set in the UI part of a shiny application
    It add a windows containing a radioButton to apply a function
    on a numeric vector.
show_data function has to be set in the Server part.
}
\examples{
\dontrun{
# In UI :
show_dataUI(id = "mod3")
# In Server
callModule(module = show_data,
    id = "mod3",
    variable = reactive(dataset$data_var_x))
}
}
