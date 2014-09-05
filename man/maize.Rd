\name{maize}
\alias{maize}
\docType{data}
\title{
Maize variety trial 
}
\description{
Maize variety trial with two years and multi-locations in China.
}
\usage{data(maize)}
\format{
  A data frame with 260 observations on the following 4 variables.
  \describe{
    \item{\code{Cultivar}}{test cultivar}
    \item{\code{Year}}{test years}
    \item{\code{Location}}{test locations}
    \item{\code{Yld}}{maize yield}
  }
}
\details{
No other details available
}
\source{
Fan X.M., Kang M.S., Chen H.M., Zhang Y.D., Tan J., Xu C.X. (2007) Yield stability of maize hybrids evaluated in multi-environment trials in Yunnan, China. Agronomy Journal.99:220-228
}
\references{
Fan X.M., Kang M.S., Chen H.M., Zhang Y.D., Tan J., Xu C.X. (2007) Yield stability of maize hybrids evaluated in multi-environment trials in Yunnan, China. Agronomy Journal.99:220-228

Finlay, K.W., G.N. Wilkinson 1963. The analysis of adaptation in a plant breeding programme. Australian Journal of Agricultural Research 14: 742-754.

Wu, J., K. Glover, W. Berzonsky, 2012. Statistical tests for stability analysis with resampling techniques.  25th Conference of Applied Statistics in Agriculture. p88-108. April 29- May 01, 2012. Manhattan, KS
}
\examples{
 
 library(qgtools)
 data(maize)
 names(maize)

  
 ##End

}
\keyword{datasets}
\keyword{stability analysis}
\keyword{maize}
\keyword{stab.mean}
\keyword{stab.var}
\keyword{stab.fw}



