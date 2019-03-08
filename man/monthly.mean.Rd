\name{monthly.mean}
\alias{monthly.mean}
\title{
Monthly mean of flow records not differentiated by year}
\description{
The function returns the monthly arithmetic mean of continuous flow records.The mean is not differentiated by year,therefore if more year of flow records are reported, the mean refers to all flow records of the same month in the different years. It is also possible to obtain the standard deviation.}
\usage{
monthly.mean(flow.data, standev)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with continuous flow records labelede "flow".}
  \item{standev}{
An optional argument. If the user specifies "sd", the function will return the standard deviation for each row}
}
\value{
A data.frame with a first column with the months (labeled "month") and a second column with monthly mean flow (labeled "flow"). If the user has specified "sd", there is a third column with standard deviation value for each row (labeles "sd").}
\author{
Veronica Nava}
\seealso{
\code{\link{daily.mean}}
\code{\link{monthly.year.mean}}
\code{\link{annual.mean}}
}
\examples{
data("flow.data2")
mon<-monthly.mean(flow.data2)
mon.sd<-monthly.mean(flow.data2, "sd")
}
\keyword{univar}
