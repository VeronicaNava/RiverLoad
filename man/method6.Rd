\name{method6}
\alias{method6}
\title{
Load estimation based on linear interpolation of concentration}
\description{
Estimate the load linearly interpolating the concentration data and then multipling the values obtained by the flow records (Moatar & Meybeck, 2005). The estimation is performed on the time period spanned by flow records. It is also possible to estimate the load monthly or annually specifying optional argument.}
\usage{
method6(db, ncomp, period)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output matrix of the function db.union.}
  \item{ncomp}{
Number of compounds in the input data frame for which the load must be estimated.}
  \item{period}{
Optional argument. It can be "month" for a monthly estimation of load, or it can be "year" for annual estimation of load. If it misses, the calculation is performed on the time period spanned by the streamflow data.}
}
\value{
A matrix with the estimated load with linear interpolation of concentration method for the different compounds. If the optional argument 'period' misses, the matrix will have only one row with the load estimation done throughout the time period spanned by streamflow data. Otherwise, if the period is specified, the load is estimated in the different months/year and the load values are returned in different rows, named with the month/year considered. The load is expressed in gram per period of estimation.
}
\references{
Moatar, F., & Meybeck, M. (2005). Compared performances of different algorithms for estimating annual nutrient loads discharged by the eutrophic River Loire. Hydrological Processes, 19(2), 429-444. https://doi.org/10.1002/hyp.5541
}
\author{
Veronica Nava}
\examples{
data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met6<-method6(union, 2)
met6.month<-method6(union, 2, "month")
met6.year<-method6(union, 2, "year")
}
\keyword{arit}
