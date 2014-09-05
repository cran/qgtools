\name{adrcdat}
\alias{adrcdat}
\docType{data}
\title{
F2 spring wheat data with row and column
}
\description{
F2 spring wheat data with row and column can be used to separate field variation. It can be integrated with a AD model using the functions:adrc.mq or adrc.mq.jack.
}
\usage{data(adrcdat)}
\format{
  A data frame with 358 observations on the following 10 variables.
  \describe{
    \item{\code{Env}}{Location code}
    \item{\code{Row}}{Field row code}
    \item{\code{Column}}{Field column}
    \item{\code{Female}}{Female parent}
    \item{\code{Male}}{Male parent}
    \item{\code{Gen}}{Generation. 0=parent and 2=F2}
    \item{\code{Rep}}{Field replication}
    \item{\code{YIELD}}{Grain yield}
    \item{\code{HEIGHT}}{Plant height}
    \item{\code{HEADING}}{Heading date}
  }
}
\details{
No other details
}
\source{
Wu J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 
}
\references{
Rao, C.R. 1971. Estimation of variance and covariance components-MINQUE theory. J Multiva Ana 1:19

Wu, J., McCarty Jr., J.C., Jenkins, J.N. 2010. Cotton chromosome substitution lines crossed with cultivars: Genetic model evaluation and seed trait analyses. Theoretical and Applied Genetics 120:1473-1483.

Wu, J., J. N. Jenkins, J. C. McCarty, K. Glover, and W. Berzonsky. 2010. Presentation titled by "Unbalanced Genetic Data Analysis: model evaluation and application" was offered at ASA, CSSA, & SSSA 2010 International Annual Meetings, Long Beach, CA.

Wu, J., J. N. Jenkins, and J.C., McCarty. 2011. A generalized approach and computer tool for quantitative genetics study. Proceedings Applied Statistics in Agriculture, April 25-27, 2010, Manhattan, KS. p.85-106. 

Wu, J. 2012. GenMod: An R package for various agricultural data analyses.  ASA, CSSA, and SSSA 2012 International Annual Meetings, Cincinnati, OH, p 127

Wu J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A
}
\examples{
  library(qgtools)
  data(adrcdat)
  names(adrcdat)

}
\keyword{datasets}
\keyword{ad model}
\keyword{jackknife}
\keyword{MINQUE}
\keyword{row effect}
\keyword{column effect}
