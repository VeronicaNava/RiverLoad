\name{flow.data2}
\alias{flow.data2}
\docType{data}
\title{
Dataset of flow record of Sandusky River}
\description{
Dataframe with two columns: a first column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labeled "flow". These latter are expressed in cubic metre per second. The dataset covers a one-year period with flow records every 1 day.
}
\usage{data("flow.data2")}
\format{
  A data frame with 365 observations on the following 2 variables.
  \describe{
    \item{\code{datetime}}{a POSIXct}
    \item{\code{flow}}{a numeric vector}
  }
}
\source{
\url{https://ncwqr.org/monitoring/data/
}}
\references{
Heidelberg Tributary Loading Program (HTLP) and USGS, United States Geological Survey
}
\examples{
data(flow.data2)
summary(flow.data2)
}
\keyword{datasets}
