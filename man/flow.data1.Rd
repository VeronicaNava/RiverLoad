\name{flow.data1}
\alias{flow.data1}
\docType{data}
\title{
Dataset of flow record of Kaskskia River}
\description{
Dataframe with two columns: a first column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labeled "flow". These latter are expressed in cubic metre per second. The dataset covers a two-year period with flow records every 1 day.}
\usage{data(flow.data1)}
\format{
  A data frame with 731 observations on the following 2 variables.
  \describe{
    \item{\code{datetime}}{a POSIXct}
    \item{\code{flow}}{a numeric vector}
  }
}
\source{
\url{https://waterdata.usgs.gov/usa/nwis/uv?05595000
}}
\references{
USGS, United States Geological Survey}
\examples{
data("flow.data1")
summary(flow.data1)
}
\keyword{datasets}
