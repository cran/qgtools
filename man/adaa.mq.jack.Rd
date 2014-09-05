\name{adaa.mq.jack}
\alias{adaa.mq.jack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ADAA model with MINQUE and jacknife analyses
}
\description{
An dditive-dominance (AD) model and additive-by-additive interaction effects (ADAA model) can be analyzed by MINQUE approach, requiring no specific genetic mating design or balance data. 
For reliable results, three generations such as (parents,F1s,F2s) or  (parents, F2s,F3s) are preferred.
The jackknife method will conduct all statistical tests.
}
\usage{
adaa.mq.jack(Y, Ped, JacNum = NULL, JacRep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A trait matrix including one or more traits.
}
  \item{Ped}{
A pedigree matrix including Env, Female, Male, Generation, with or without block is required. So the matrix should include either 4 columns or 5 columns.
}
  \item{JacNum}{
Number of jackknife groups. The default is 10.
}
  \item{JacRep}{
Repeating times for jackknife process. The default is 1.
}
}
\details{
A pedigree matrix used for analysis is required in the order of Environment (column 1), Female(column 2), Male(column 3), Generation (column 4). Column 5 for block can be default. Even though there is only one environment, this column is needed.
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

Wu, J., J. N. Jenkins, Jack C. McCarty, and D. Wu. 2006b Variance component estimation using the ADAA model when genotypes vary across environments. Crop Science 46: 174-179.

Wu, J., J. N. Jenkins, J. C. McCarty, K. Glover, and W. Berzonsky. 2010. Presentation titled by "Unbalanced Genetic Data Analysis: model evaluation and application" was offered at ASA, CSSA, & SSSA 2010 International Annual Meetings, Long Beach, CA.

Wu, J., J. N. Jenkins, and J.C., McCarty. 2011. A generalized approach and computer tool for quantitative genetics study. Proceedings Applied Statistics in Agriculture, April 25-27, 2010, Manhattan, KS. p.85-106. 

Wu, J. 2012. GenMod: An R package for various agricultural data analyses.  ASA, CSSA, and SSSA 2012 International Annual Meetings, Cincinnati, OH, p 127

Wu J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A
}
\author{
Jixiang Wu <qgtools@gmail.com>
}
\examples{

 library(qgtools)

 data(cotf12)
 names(cotf12)
 dat=cotf12[which(cotf12$Year==1),]
 Ped=dat[,c(1,3:6)]
 Y=dat[,-c(1:6)]
 #Ped=cotf12[,c(1,3:6)]
 #Y=cotf12[,-c(1:6)]
 
 res=adaa.mq.jack(Y,Ped,JacNum=5)
 res$Var
 res$FixedEffect
 res$RandomEffect

 ##End
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ADAA model }
\keyword{ minque }
\keyword{ jaccknife }% __ONLY ONE__ keyword per line
\keyword{ cotf12 }
