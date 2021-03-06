% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_scale.R
\name{apply_scale}
\alias{apply_scale}
\title{apply_scaleUI}
\usage{
apply_scale(input, output, session, variable = NULL)
}
\arguments{
\item{input}{Not a real parameter, should not be set manually. Done by callModule automatically.}

\item{output}{Not a real parameter, should not be set manually. Done by callModule automatically.}

\item{session}{Not a real parameter, should not be set manually. Done by callModule automatically.}

\item{variable}{Numeric. Vector containing dataset to apply function on.}
}
\value{
Server logic
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
data_module4   <- callModule(module = apply_scale, id = "mod4",
                      variable = reactive(rv$variable))
}
}
