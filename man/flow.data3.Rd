\name{flow.data3}
\alias{flow.data3}
\docType{data}
\title{
Flow record dataset of Adda River}
\description{
Dataframe with two colmns: a first column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labeled "flow". These latter are expressed in cubic metre per second. The dataset covers a one-year period with flow records every 12 hours.}
\usage{data("flow.data3")}
\format{
  A data frame with 728 observations on the following 2 variables.
  \describe{
    \item{\code{datetime}}{a POSIXct}
    \item{\code{flow}}{a numeric vector}
  }
}
\source{
\url{http://www.arpalombardia.it/siti/arpalombardia/meteo/richiesta-dati-misurati/Pagine/RichiestaDatiMisurati.aspx
}}
\references{
ARPA-Lombardia, Italian Environmental Protection Agency}
\examples{
data("flow.data3")
str(flow.data3)
}
\keyword{datasets}
