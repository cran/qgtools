\name{stab.fw}
\alias{stab.fw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resgression based stability analysis
}
\description{
A simple and multiple linear regression based method for stability analysis
}
\usage{
stab.fw(y, Gen, Env, times, Rep, X = NULL, alpha = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
   A response varible vector used for stability analysis
}
  \item{Gen}{
  A vector of genotypes
}
  \item{Env}{
  A vector of environments
}
  \item{times}{
  Times of resampling used for stability analysis.
}
  \item{Rep}{
An argument with replication: Rep=TRUE or with replication: Rep=FALSE
}
  \item{X}{
A vector or matrix of other predictable variables. Default is NULL.
}
  \item{alpha}{
A nominal probability values used for statistical tests
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\value{
Return the stability parameter estimates and their confidence interval of (1-alpha) for each genotype
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Finlay, K.W., G.N. Wilkinson 1963. The analysis of adaptation in a plant breeding programme. Australian Journal of Agricultural Research 14: 742-754.

Wu, J., K. Glover, W. Berzonsky, 2012. Statistical tests for stability analysis with resampling techniques.  25th Conference of Applied Statistics in Agriculture. p88-108. April 29- May 01, 2012. Manhattan, KS
}
\author{
Jixiang Wu <qgtools@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
stab.mean

stab.var

stab.ammi

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
 
 library(qgtools)
 data(maize)
 #names(maize)

 Geno=as.vector(maize$Cultivar)
 Env=paste(maize$Location,maize$Year,sep=":")
 y=maize$Yld

 res=stab.fw(y,Gen=Geno,Env=Env,times=10,Rep=TRUE)
 res

 ##end

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ stability }
\keyword{ resampling }
\keyword{ maize }
\keyword{ fw stability analysis }% __ONLY ONE__ keyword per line
