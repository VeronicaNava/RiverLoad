\name{db.union}
\alias{db.union}
\title{
Union of flow and concentration data in a unique data frame}
\description{
This function allows merging the data maintaining all the available flow records with the scattered concentration values on the basis of DateTime information}
\usage{
db.union(flow.data, conc.data)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with continuous flow records labelede "flow".}
  \item{conc.data}{
A dataframe with a first column with DateTime information in standard format (yyyy-mm-dd HH:MM:SS) and the following columns with concentration of different compounds in milligram per litre.}
}
\value{
A dataframe with three columns: the first column with DateTime in standard format (yyyy-mm-dd HH:MM:SS), the second column with continuous flow records, and the third column with scattered concentration data, paired on the basis of datetime column. All the flow data are mainteined. For the DateTime in which concentration data are not available, 'NA' is reported.
}
\author{
Veronica Nava}
\seealso{
\code{\link{db.intersect}}
}
\examples{
data("flow.data2", "conc.data2")
intersect<-db.intersect(flow.data2, conc.data2)
summary<-db.intersect
}
\keyword{manip}
\keyword{datagen}

