\name{monthly.year.mean}
\alias{monthly.year.mean}
\title{
Monthly mean of flow records differentiated by year}
\description{
The function returns the monthly arithmetic mean of flow records. The mean is differentiated by year,therefore if more years of flow records are reported, the function returns separately the mean for every single month of each year. It is also possible to obtain the standard deviation.}
\usage{
monthly.year.mean(flow.data, standev)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labelede "flow".}
  \item{standev}{
An optional argument. If the user specifies "sd", the function will return the standard deviation for each row}
}
\value{
A data.frame with a first column with the months (labeled "month") and a second column with monthly mean flow (labeled "flow"). If the user has specified "sd", there is a third column with standard deviation value for each row (labeles "sd").}
\author{
Veronica Nava}

\examples{
data("flow.data1")
mon<-monthly.mean(flow.data1)
mon.sd<-monthly.mean(flow.data1, "sd")
}

\keyword{univar}
