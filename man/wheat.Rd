\name{wheat}
\alias{wheat}
\docType{data}
\title{
Multi-parental mating design data
}
\description{
 A F2 what data set includes parents, two-way, three-way, and four-way crosses.
}
\usage{data(wheat)}
\format{
  A data frame with 802 observations on the following 8 variables.
  \describe{
    \item{\code{Env}}{Code for year}
    \item{\code{P1}}{Codes for female 1}
    \item{\code{P2}}{Codes for male 1}
    \item{\code{P3}}{Codes for female 2}
    \item{\code{P4}}{Code for male 2}
    \item{\code{Gen}}{Codes for generation: 0=parent and 2=F2}
    \item{\code{REP}}{Codes for replication}
    \item{\code{Average}}{Pre-harvest sprout index}
  }
}
\details{
 No other details available
}
\source{
 Not available
}
\references{
 To be added
}
\examples{

 library(qgtools)
 data(wheat)
 summary(wheat)


}
\keyword{datasets}
