\name{annual.mean}
\alias{annual.mean}
\title{
Annual mean of flow records}
\description{
The function returns the annual arithmetic mean of flow records. It is also possible to obtain the standard deviation.}
\usage{
annual.mean(flow.data, standev)
}
\arguments{
  \item{flow.data}{
A dataframe with a first column with DateTime information in standard format(yyyy-mm-dd HH:MM:SS) labeled "datetime" and a second column with flow records labelede "flow".}
  \item{standev}{
An optional argument. If the user specifies "sd", the function will return the value of the standard deviation for each row.}
}

\value{
A data frame with two columns: the first with the years (labeled "year") and the second with annual mean flow data (labeled "flow"). If the user has specified "sd", there is a third column with standard deviation value for each row (labeled "sd").}

\author{
Veronica Nava}

\seealso{
\code{\link{daily.mean}}
\code{\link{monthly.mean}}
\code{\link{monthly.year.mean}}
}
\examples{
data("flow.data1")
annual.mean(flow.data1)
annual.mean(flow.data1, "sd")
}

\keyword{univar}
