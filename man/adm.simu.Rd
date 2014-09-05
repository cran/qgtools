\name{adm.simu}
\alias{adm.simu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An R function for ADM model simulation.
}
\description{
An R function for ADM model simulation with generated data set.
}
\usage{
adm.simu(Y, Ped, method = NULL, ALPHA = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A matrix of simulated data set
}
  \item{Ped}{
A pedigree matrix including Environment, Female, Male, Generation, with or without block is required. So the matrix should include either 4 columns or 5 columns.
}
  \item{method}{
The default linear mixed model approach is MINQUE. Users can choose both or one of two linear mixed model approaches, REML and MINQUE.
}
  \item{ALPHA}{
A preset nominal probability level.The default is 0.05.
}
}

\value{
Return list of simulated results for variance components.
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
 
  dat=cotf2[which(cotf2$Env==1),]
  Ped=dat[,c(1:5)]
  Y=dat[,-c(1:5)]
  YS=adm.simudata(Y,Ped,v=rep(20,6),b=c(100),SimuNum=10)
  res=adm.simu(YS,Ped)
  res
  ##End


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ADM model }
\keyword{ REML }
\keyword{ MINQUE }
\keyword{ cotton }

