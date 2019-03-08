\name{daily.mean}
\alias{daily.mean}
\title{
Daily mean of flow records}
\description{
The function returns the daily arithmetic mean of flow records.}
\usage{
daily.mean(flow.data, standev)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labelede "flow".}
  \item{standev}{
An optional argument. If the user specifies "sd", the function will return the standard deviation for each row.}
}

\value{
A data frame with two columns: the first with the day and the second with annual mean flow data (labeled "flow"). If the user has specified "sd", there is a third column with standard deviation value for each row (labeled "sd").}

\author{
Veronica Nava}

\seealso{
\code{\link{annual.mean}}
\code{\link{monthly.mean}}
\code{\link{monthly.year.mean}}
}

\examples{
data("flow.data1")
daily.mean(flow.data1)
daily.mean(flow.data1, "sd")}

\keyword{univar}
