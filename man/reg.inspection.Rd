\name{reg.inspection}
\alias{reg.inspection}
\title{
Parameters of regression analysis between flow and concentration}
\description{
Calculate regression analysis parameters. The output returns the slope and intercept coefficients and their related p-value, the R^2, the adjusted R^2, and the residual degrees-of-freedom}
\usage{
reg.inspection(db, ncomp)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output matrix of the function db.union.}
  \item{ncomp}{
Number of compounds in the input data frame for which the load must be estimated.}}
\value{
A matrix with the slope coefficient, the slope p-value, the intercept coefficient, the intercept p-value, the R^2, and the residual degrees-of-freedom for each compound.
}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
\code{\link{rating}}
\code{\link{ferguson}}
}
\examples{
data("flow.data3", "conc.data3")
union<-db.union(flow.data3, conc.data3)
reg.parametrs<-reg.inspection(union, 1)
}
\keyword{regression}
