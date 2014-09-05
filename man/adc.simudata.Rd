\name{adc.simudata}
\alias{adc.simudata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An R function to generate an ADC model simulated data set
}
\description{
An R function to generate an ADC model simulated data set with given parameters and data structure.
}
\usage{
adc.simudata(Y, Ped, v, b, SimuNum = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A matrix of trait with one or more than one trait.
}
  \item{Ped}{
A pedigree matrix including Environment, Female, Male, Generation, with or without block is required. So the matrix should include either 4 columns or 5 columns.
}
  \item{v}{
A vector of preset variance components.
}
  \item{b}{
A vector of present fixed effects.
}
  \item{SimuNum}{
The number of simulations. The default number is 200.
}
}

\value{
Return a simulated data set which is a matrix.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Rao, C.R. 1971. Estimation of variance and covariance components-MINQUE theory. J Multiva Ana 1:19

Rao, C. R. and Kleffe, J. 1980. Estimation of variance components. In Handbook of Statistics. Vol. l: 1-40. Krishnaiah, P. R. ed. New York. North-Holland.

Searle, S. R., Casella, G. and McCulloch, C. E. 1992. Variance Components. John Wiley & Sons, Inc. New York.

Wu J (2012) GenMod: An R package for various agricultural data analyses.  ASA, CSSA, and SSSA 2012 International Annual Meetings, Cincinnati, OH, p 127

Wu J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A

}
\author{
Jixiang Wu <qgtools@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
library(qgtools)
 data(cotf2)
 Ped=cotf2[,c(1:5)]
 Y=cotf2[,-c(1:5)]
 
 YS=adc.simudata(Y,Ped,v=rep(20,9),b=c(100))
 
 ##End

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ADC model }
\keyword{ cotton }
\keyword{ simuated data }
\keyword{ cotf2 }
