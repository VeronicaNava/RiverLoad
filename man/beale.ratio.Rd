\name{beale.ratio}
\alias{beale.ratio}
\title{
Load estimation with Beale ratio method}
\description{
Estimate the load using the Beale ratio methods (Quilbe' et al., 2006). The estimation is performed on the time period spanned by flow records. It is also possible to estimate the load monthly or annually specifying optional argument. However, the bias correction factor is still calculated over the entire period.}
\usage{
beale.ratio(db, ncomp, period)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with continuous flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output matrix of the function db.union.}
  \item{ncomp}{
Number of compounds in the input data frame for which the load must be estimated.}
  \item{period}{
Optional argument. It can be "month" for a monthly estimation of load, or it can be "year" for annual estimation of load. If it misses, the calculation is performed on the time period spanned by the streamflow data.}
}
\value{
A matrix with the estimated load with Beale ratio for the different compounds. If the optional argument 'period' misses, the matrix will have only one row with the load estimation done throughout the time period spanned by streamflow data. Otherwise, if the period is specified, the load is estimated in the different months/year and the load values are returned in different rows, named with the month/year considered. The load is expressed in gram per period of estimation.}

\references{
Quilbe', R., Rousseau, A. N., Duchemin, M., Poulin, A., Gangbazo, G., & Villeneuve, J. P. (2006). Selecting a calculation method to estimate sediment and nutrient loads in streams: Application to the Beaurivage River (Quebec, Canada). Journal of Hydrology, 326(1-4), 295-310. https://doi.org/10.1016/j.jhydrol.2005.11.008
}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
}
\examples{
data("flow.data1", "conc.data1")
union<-db.union(flow.data1, conc.data1)
beale<-beale.ratio(union, 2)
beale.month<-beale.ratio(union, 2, "month")
beale.year<-beale.ratio(union, 2, "year")
}
\keyword{univ}
