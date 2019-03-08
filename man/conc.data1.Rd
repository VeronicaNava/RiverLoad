\name{conc.data1}
\alias{conc.data1}
\docType{data}
\title{
Concentration dataset of Kaskaskia River}
\description{
Scattered concentration data of nitrite plus nitrate ("NOx") and soluble reactive phosphorus ("SRP") of Kaskaskia River, IL. The data are expressed in mg/L and covered a two-year period from January 2016 to December 2017.}
\usage{data("conc.data1")}
\format{
  A data frame with 130 observations on the following 3 variables.
  \describe{
    \item{\code{datetime}}{a POSIXct}
    \item{\code{NOx}}{a numeric vector}
    \item{\code{SRP}}{a numeric vector}
  }
}
\source{
\url{https://waterdata.usgs.gov/usa/nwis/uv?05595000
}}
\references{
USGS, United States Geological Survey}
\examples{
data("conc.data1")
str(conc.data1)
summary(conc.data1)
}
\keyword{datasets}
