\name{stab.mean}
\alias{stab.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group mean and rank calculator
}
\description{
A function to calculate group means and ranks with two resampling techniques
}
\usage{
stab.mean(Y, class, cls2 = NULL, resample, times = NULL, alpha = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A matrix including One or more traits
}
  \item{class}{
   A vector of the first factor for calculating variance. For example, a vector of genotypes.
}
  \item{cls2}{
   A vector of the second factor used within-group bootstraping for variance.  It can be default
}
  \item{resample}{
Resampling technique option. resample="Boot" is for bootstrapping. resample="Perm" is for permutation.

}
  \item{times}{
Number of resampling used. The default is 1000.
}
  \item{alpha}{
A nomial probability used for statistical test. The default value is 0.05.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Return ranks and means and their(1-alpha) confidence intervals
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


\examples{
 
 ##Sample R codes for the function genmod.rank
 library(qgtools)
 data(maize)
 #names(maize)

 Geno=as.vector(maize$Cultivar)
 Env=paste(maize$Location,maize$Year,sep=":")
 y=maize$Yld

 res=stab.mean(y,class=Geno,cls2=Env,resample="Boot",times=10)
 res

 res=stab.mean(y,class=Geno,resample="Perm",times=10)
 res

 ##end


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stab.mean }
\keyword{ stability }
\keyword{ rank }
\keyword{ mean }
\keyword{ resampling }
\keyword{ bootstrapping }
\keyword{ permutation }
\keyword{ maize }% __ONLY ONE__ keyword per line
