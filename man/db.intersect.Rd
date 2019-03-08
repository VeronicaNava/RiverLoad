\name{db.intersect}
\alias{db.intersect}
\title{
Intersection of flow and concentration data in a unique data frame}
\description{
Flow and concentration data are merged in a unique data frame on the basis of DateTime information. The function pairs the data maintaining the rows in which both the concentration and the flow records are available. None ‘NA' value is reported }
\usage{
db.intersect(flow.data, conc.data)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with continuous flow records labelede "flow".}
  \item{conc.data}{
A dataframe with a first column with DateTime information in standard format (yyyy-mm-dd HH:MM:SS) and the following columns with concentration of different compounds in milligram per litre.}
}
\value{
A dataframe with three columns: the first column with DateTime in standard format (yyyy-mm-dd HH:MM:SS), the second column with flow records, and the third column with concentration data, paired on the basis of datetime column. Only the rows in which both concentration and flow data are available are maintened.
}
\author{
Veronica Nava}
\seealso{
\code{\link{db.union}}
}
\examples{
data("flow.data2","conc.data2")
intersect<-db.intersect(flow.data2, conc.data2)
summary<-db.intersect
}
\keyword{manip}
\keyword{datagen}
