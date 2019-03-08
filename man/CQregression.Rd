\name{CQregression}
\alias{CQregression}
\title{
Relationship between concentration and flow}
\description{
Calculate the R^2 of the relationship between concentration and flow. Statistically significant correlation between concentration and flow is mandatory to perform analysis using regression methods (Quilbe' et al., 2006; Aulenbach et al., 2016).}
\usage{
CQregression(db, ncomp)
}
\arguments{
  \item{db}{
An input data frame with at least three column. A column with DateTime in standard format (yyyy-mm-dd HH:MM:SS) labeled "datetime", a column with continuous flow records in cubic meter per second labeled "flow", and columns with scattered concentration data in milligram per litre. Alternatively, it can be used the output data frame of the function db.union.}
  \item{ncomp}{
Number of compounds in the input data frame for which the load must be estimated.}
}
\value{
A matrix with R^2 for each compound.}
\references{
Quilbe', R., Rousseau, A. N., Duchemin, M., Poulin, A., Gangbazo, G., & Villeneuve, J. P. (2006). Selecting a calculation method to estimate sediment and nutrient loads in streams: Application to the Beaurivage River (Quebec, Canada). Journal of Hydrology, 326(1-4), 295-310. https://doi.org/10.1016/j.jhydrol.2005.11.008
Aulenbach, B. T., Burns, D. A., Shanley, J. B., Yanai, R. D., Bae, K., Wild, A. D.,...,Yi, D. (2016). Approaches to stream solute load estimation for solutes with varying dynamics from five diverse small watersheds. Ecosphere, 7(6), 1-22. https://doi.org/10.1002/ecs2.1298
}
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
reg.relationship<-CQregression(union, 2)
}

\keyword{regression}
