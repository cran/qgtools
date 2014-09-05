\name{cotf2}
\alias{cotf2}
\docType{data}
\title{
  A cotton F2 data set from a 2*6 factorial genetic mating design. 
}
\description{
 Twelve F2 hybrids and their 8 parents were evaluated under two years each an randomized complete block design. 
}
\usage{data(cotf2)}
\format{
  A data frame with 240 observations on the following 9 variables.
  \describe{
    \item{\code{Env}}{Codes for years}
    \item{\code{Female}}{Codes for female parents}
    \item{\code{Male}}{Codes for male parents}
    \item{\code{Gen}}{Codes for generations: 0=parent, 2=F2}
    \item{\code{rep}}{Codes for field block within each year}
    \item{\code{BN}}{Boll number}
    \item{\code{BS}}{Boll size}
    \item{\code{LP}}{Lint percentage}
    \item{\code{LY}}{Lint yield}
  }
}
\details{
 Please refer to the example R codes for more information. This data set can be analyzed by different models.
}
\source{
 Not availble
}
\references{
 No reference available.
}
\examples{
 library(qgtools)
 data(cotf2)
 names(cotf2)
 
 #End
}
\keyword{datasets}
\keyword{AD model}
\keyword{ADC model}
\keyword{ADM model}
\keyword{MINQUE}
\keyword{jackknife}
\keyword{genetic models}
\keyword{ lint yield }
\keyword{ lint percentage}
\keyword{ boll number }
\keyword{ lint size }

