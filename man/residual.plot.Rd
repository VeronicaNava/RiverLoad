\name{residual.plot}
\alias{residual.plot}
\title{
Residual plots of one selected component}
\description{
Obtain the diagnostic plots returned by plot.lm: a plot of residuals against fitted values, a Scale-Location plot of squared root of absolute value of residuals against fitted values, a Normal Q-Q plot, a plot of residuals against leverages.}
\usage{
residual.plot(db, numbercomponent, filepath)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output matrix of the function db.union.}
  \item{numbercomponent}{
The position number of the specific compounds for which the user would obtain the plots, as the function returns the graphs for one compound at time.
}
\item{filepath}{
An optional argument. The user can directly specify the path in which the plot must be saved. If this argument misses, the plot is showed in R window.}
}
\value{
A graphical ouput with plots of residuals against fitted values, a Scale-Location plot of squared root of absolute value of residuals against fitted values, a Normal Q-Q plot, a plot of residuals against leverages.}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
\code{\link{rating}}
\code{\link{ferguson}}
}
\examples{
data("flow.data1","conc.data1")
union<-db.union(flow.data1, conc.data1)
residual.plot(union, 1)
}
\keyword{regression}
\keyword{dplot}
