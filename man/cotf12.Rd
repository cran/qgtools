\name{cotf12}
\alias{cotf12}
\docType{data}
\title{
Cotton F1 and F2 data
}
\description{
  A cotton data dat set contains 10 parents, 20 F1 hybrids, and 20 F2 hybrids, which were evaluated at one research at Zhejiang Agricultural Univsersity in 1992 and 1993.
}
\usage{data(cotf12)}
\format{
  A data frame with 300 observations on the following 11 variables.
  \describe{
    \item{\code{Year}}{Codes for years}
    \item{\code{Entry}}{Codes for 50 entries}
    \item{\code{Female}}{Codes for female parents}
    \item{\code{Male}}{Codes for male parents}
    \item{\code{Gen}}{Codes for generations: 0=parent, 1=F1, and 2=F2}
    \item{\code{Blk}}{Code for field blocks}
    \item{\code{CtYld}}{Cotton seed yield}
    \item{\code{TlnY}}{Total lint yield}
    \item{\code{LintY}}{Lint yield}
    \item{\code{Bolls}}{Boll numeric per plant}
    \item{\code{Bsize}}{Boll size}
  }
}
\details{
  This data set can be analyzed by different genetic models: AD, ADC, ADM, and ADAA models as showed in the examples.
}
\source{
  Not available
}
\references{
  To be added
}
\examples{
 library(qgtools)
 data(cotf12)
 names(cotf12)
 
 #End
}
\keyword{datasets}
\keyword{cotton}
\keyword{F1}
\keyword{F2}
\keyword{AD model}
\keyword{ADC model}
\keyword{ADM model}
\keyword{ADAA model}
\keyword{MINQUE}
\keyword{jackknife}




