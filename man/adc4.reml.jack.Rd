\name{adc4.reml.jack}
\alias{adc4.reml.jack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ADC model, multi-parent mating designs, REML, and jackknife
}
\description{
An ADC model from multi-parent mating designs can be analyzed by REML and jackknife methods, requiring no specific genetic mating designs or balance data. 
}
\usage{
adc4.reml.jack(Y, Ped, JacNum = NULL, JacRep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A trait matrix including one or more than one traits.
}
  \item{Ped}{
A pedigree matrix including Environment, Female1, Male1, Female2, Male2, Generation, with or without block is required. So the matrix should include either 6 columns or 7 columns.
}
  \item{JacNum}{
Number of groups to be jackknifed. The default is 10
}
  \item{JacRep}{
Number of jackknife process to be repeated. The default is 1.
}
}
\details{
A pedigree matrix used for analysis is required in the order of Environment (column 1), Female1(column 2), Male1(column 3), Female2(column 4), Male2(column 5),Generation (column 6). Column 7 for block can be default. Even though there is only one environment, this column is needed.
}
\value{
Return a list of results: estimated variance components, estimated proportional variance components, estimated fixed  effects, and predicted random effects
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

Wu J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A
}
\author{
Jixiang Wu <qgtools@gmail.com>
}

\examples{

 library(qgtools)
 data(wheat)
 n=nrow(wheat)
 id=sample(n,200)
 dat=wheat[id,]
 Ped=dat[,c(1:6)]
 Y=as.matrix(dat[,8])
 colnames(Y)=colnames(dat)[8]
 
 res=adc4.reml.jack(Y,Ped,JacNum=5)
 res$Var
 res$PVar
 res$FixedEffect
 res$RandomEffect

 ##End

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ADC model }
\keyword{ multi-parent mating design }
\keyword{ REML }
\keyword{ wheat }
\keyword{ jackknife }
