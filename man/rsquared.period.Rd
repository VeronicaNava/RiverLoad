\name{rsquared.period}
\alias{rsquared.period}
\title{
Coefficient of determination for period based regression analyses}
\description{
Return the coefficient of determination (R^2) to inspect the relationship between concentration and flow. It is a useful tool for the function rating.period and ferguson.period.}
\usage{
rsquared.period(db, ncomp, period)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output matrix of the function db.union.}
  \item{ncomp}{
Number of compounds in the input data frame for which the load must be estimated.}
  \item{period}{
A mandatory argument that specify the time period in which the regression relationship must be calculated. It can be "month" for a monthly estimation of load, or it can be "year" for annual estimation of load.}
}
\value{
A matrix with the R^2 value for the different compiunds.}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
\code{\link{rating.period}}
\code{\link{ferguson.period}}
}
\examples{
data("flow.data3","conc.data3")
union<-db.union(flow.data3, conc.data3)
rsquared.period(union,1, "month")
rsquared.period(union,1, "year")
}

\keyword{regression}
