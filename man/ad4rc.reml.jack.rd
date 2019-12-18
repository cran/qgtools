\name{ad4rc.reml.jack}
\alias{ad4rc.reml.jack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AD model with row and column effects analyzed by MINQUE and jackknife
}
\description{
An AD model with row and column effects included is used for controlling field variation. This model will be analyzed by MINQUE approach and tested by jackknife technique. The data set can be irregular or missing but the field layout should be rectangular. It can analyze any genetic mating designs and data including F1, F2, or F3 with parents..
}
\usage{
ad4rc.reml.jack(Y, Ped, Row = NULL, Col = NULL, JacNum = NULL, JacRep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A data matrix for one or more traits
}
  \item{Ped}{
A pedigree matrix including Environment, Female1, Male1, Female2, Male2, Generation is required. 
}
  \item{Row}{
A vector for field rows. It can be default.
}
  \item{Col}{
A vector for field colums.It can be default.
}
  \item{JacNum}{
Number of jackknife groups to be used. The default is 10.
}
  \item{JacRep}{
Repeating times for jackknife process. The default is 1.
}
}
\details{
A pedigree matrix used for analysis is required in the order of Environment (column 1), Female(column 2), Male(column 3), Generation (column 4). Even though there is only one environment, this first column is needed.If only row or column vector is included, this is equivallent to an AD model with block effects.
}
\value{
Return a list of results: estimated Variance components, estimated proportional variance components, estimated fixed  effects, and predicted random effects, and their statistical tests
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Rao, C.R. 1971. Estimation of variance and covariance components-MINQUE theory. J Multiva Ana 1:19

Wu, J., McCarty Jr., J.C., Jenkins, J.N. 2010. Cotton chromosome substitution lines crossed with cultivars: Genetic model evaluation and seed trait analyses. Theoretical and Applied Genetics 120:1473-1483.

Wu, J., J. N. Jenkins, J. C. McCarty, K. Glover, and W. Berzonsky. 2010. Presentation titled by "Unbalanced Genetic Data Analysis: model evaluation and application" was offered at ASA, CSSA, & SSSA 2010 International Annual Meetings, Long Beach, CA.

Wu, J., J. N. Jenkins, and J.C., McCarty. 2011. A generalized approach and computer tool for quantitative genetics study. Proceedings Applied Statistics in Agriculture, April 25-27, 2010, Manhattan, KS. p.85-106. 

Wu, J. 2012. GenMod: An R package for various agricultural data analyses.  ASA, CSSA, and SSSA 2012 International Annual Meetings, Cincinnati, OH, p 127

Wu, J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu, J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A
}
\author{
Jixiang Wu <qgtools@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
  library(qgtools)
  data(adrcdat)
  dat=adrcdat[which(adrcdat$Env==1&adrcdat$Row<=3),]
  Ped=dat[,c(1,4,5,6)]
  Y=as.matrix(dat[,8])
 
  colnames(Y)=colnames(dat)[8]
  
  Row=dat$Row
  Col=dat$Column

  
  ##run AD model with field row and column effects 
  res=adrc.reml.jack(Y,Ped,Row=Row,JacNum=5) 
  res$Var
  res$PVar
  res$FixedEffect
  res$RandomEffect
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ad model}
\keyword{REML}
\keyword{row effect}
\keyword{column effect}
\keyword{jackknife}