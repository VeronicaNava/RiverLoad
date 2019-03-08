\name{conc.data2}
\alias{conc.data2}
\docType{data}
\title{
Concentration dataset of Sandusky River
}
\description{
Scattered concentration data of total phosphorus ("TP") of Sandusky River, Ohio. The data are expressed in mg/L and covered a one-year period from January 2017 to December 2017.
}
\usage{data("conc.data2")}
\format{
  A data frame with 104 observations on the following 2 variables.
  \describe{
    \item{\code{datetime}}{a POSIXct}
    \item{\code{TP}}{a numeric vector}
  }
}
\source{
\url{https://ncwqr.org/monitoring/data/
}}
\references{
Heidelberg Tributary Loading Program (HTLP) and USGS, United States Geological Survey
}
\examples{
data("conc.data2")
str(conc.data2)
summary(conc.data2)
}
\keyword{datasets}
