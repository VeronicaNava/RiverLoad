\name{method3}
\alias{method3}
\title{
Load estimation with mean discharge-weighted concentration method}
\description{
Estimate the load with the mean discharge-weighted concentration method (Moatar & Meybeck, 2005). The estimation is performed on the time period spanned by flow records. It is possible to estimate the load also monthly or annually.}
\usage{
method3(db, ncomp, period)
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
A matrix with the estimated load with mean discharge-weighted concentration method for the different compounds. If the optional argument 'period' misses, the matrix will have only one row with the load estimation done throughout the time period spanned by streamflow data. Otherwise, if the period is specified, the load is estimated in the different months/year and the load values are returned in different rows, named with the month/year considered. The load is expressed in gram per period of estimation.
}
\references{Moatar, F., & Meybeck, M. (2005). Compared performances of different algorithms for estimating annual nutrient loads discharged by the eutrophic River Loire. Hydrological Processes, 19(2), 429-444. https://doi.org/10.1002/hyp.5541}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
}
\examples{
data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
met3<-method3(union, 2)
met3.month<-method3(union, 2, "month")
met3.year<-method3(union, 2, "year")
}
\keyword{univ}
