\name{ad.mq}
\alias{ad.mq}

\title{
Additive-dominance (AD) model with MINQUE analysis
}
\description{
An AD model can be analyzed by MINQUE approach, requiring no specific genetic mating designs or balance data. For reliable results, parents and F1s, 
parents and F2s, are preferred.
}
\usage{
ad.mq(Y, Ped)
}

\arguments{
  \item{Y}{
A trait matrix including one or more than one traits.
}
  \item{Ped}{
A pedigree matrix including Environment, Female, Male, Generation, with or without block is required. So the matrix should include either 4 columns or 5 columns.
}
}
\details{
A pedigree matrix used for analysis is required in the order of Environment (column 1), Female(column 2), Male(column 3), Generation (column 4). Column 5 for block can be default. 
Even though there is only one environment, this column is needed.
}
\value{
Return a list of results: estimated variance components, estimated fixed  effects, and predicted random effects.

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
 data(cotf2)
 Ped=cotf2[,c(1:5)]
 Y=cotf2[,-c(1:5)]
 
 res=ad.mq(Y,Ped)
 res$Var
 res$FixedEffect
 res$RandomEffect

 ##End

}
\keyword{ AD model }
\keyword{ cotton }
\keyword{ MINQUE }
\keyword{ cotf2 }
