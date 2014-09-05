\name{qgtools-package}
\alias{qgtools-package}
\alias{qgtools}
\docType{package}
\title{
Tools for Quantitative Genetics Analyses
}
\description{
We integrated with two linear mixed model approaches (MIQNUE and REML) and several resampling techniques for various genetics models. With these two types of approaches, various unbalanced data structures, missing data, and any irregular genetic mating designs can be analyzed and statistically tested. This package also offers fast computations for many large data sets.
}
\details{
\tabular{ll}{
Package: \tab qgtools\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-05\cr
License: \tab GPL -2\cr
}
The current version includes two major components: (1) genetic model analyses for various genetic mating genetic and breeding data and (2) yield stability. Both components have been integrated with resampling approaches. Regarding the first component, two linear mixed model approaches, MINQUE and REML are employed to analyze various genetic mating designs and data structures. The adjusted unbiased prediction (AUP) method is employed to predict random effects (Zhu, 1993). Functions for model/data evaluations are provided too. Randomized group-based jackknife technique is integrated for various statistical tests such as for variance components, fixed effects, and random effects (Wu et al., 2012). Four commonly used genetic models: AD (additive-dominance), ADC (AD model with cytoplasmic effects), ADM (AD model with maternal effects), and ADAA (AD model with additive-by-additive interaction effects), are provided. For the second component, we add regression based yield stability with integration of bootstrapping and permutation tests included.  Genotypic means, ranks, and variations across test environments can be calculated with resampling techniques integrated. Please refer to each function and its example R codes and data sets for detailed information and better understanding. More functions for genetic data analyses will be provided in the future.
}
\author{
Jixiang Wu: Plant Science Department, South Dakota State University, Brookings, SD 57007

Maintainer: Jixiang Wu <qgtools@gmails.com>
}
\references{

Crossa J., Gauch H.G. and Zobel R.W. (1990) Additive main effect and multiplicative interaction analysis of two international maize cultivar trials. Crop Sci 30:493-500.

Finlay, K.W., G.N. Wilkinson 1963. The analysis of adaptation in a plant breeding programme. Australian Journal of Agricultural Research 14: 742-754.

Miller, R. G. 1974. The jackknife - a review. Biometrika, 61:1-15.

Patterson, H. D. and Thompson, R. 1971. Recovery of inter-block information when block sizes are unequal. Biometrika, 58: 545-554.

Rao, C.R. 1971. Estimation of variance and covariance components-MINQUE theory. J Multiva Ana 1:19

Rao, C. R. and Kleffe, J. 1980. Estimation of variance components. In Handbook of Statistics. Vol. l:1-40. Krishnaiah, P. R. ed. New York. North-Holland.

Searle, S. R., Casella, G. and McCulloch, C. E. 1992. Variance Components. John Wiley & Sons, Inc. New York.

Wu, J., McCarty Jr., J.C., Jenkins, J.N. 2010. Cotton chromosome substitution lines crossed with cultivars: Genetic model evaluation and seed trait analyses. Theoretical and Applied Genetics 120:1473-1483.

Wu, J., K. Glover, W. Berzonsky, 2012. Statistical tests for stability analysis with resampling techniques.  25th Conference of Applied Statistics in Agriculture. p88-108. April 29-May 01, 2012. Manhattan, KS

Wu, J., J. N. Jenkins, J. C. McCarty, K. Glover, and W. Berzonsky. 2010. Unbalanced Genetic Data Analysis: model evaluation and application. 2010 ASA, CSSA, & SSSA  International Annual Meetings, Long Beach, CA.

Wu, J., J. N. Jenkins, and J.C., McCarty. 2011. A generalized approach and computer tool for quantitative genetics study. Proceedings Applied Statistics in Agriculture, April 25-27, 2010, Manhattan, KS. p.85-106. 

Wu, J. 2012. GenMod: An R package for various agricultural data analyses.  ASA, CSSA, and SSSA 2012 International Annual Meetings, Cincinnati, OH, p 127

Wu, J., Bondalapati K., Glover K., Berzonsky W., Jenkins J.N., McCarty J.C. 2013. Genetic analysis without replications: model evaluation and application in spring wheat. Euphytica. 190:447-458 

Zhu, J. 1989. Estimation of Genetic Variance Components in the General Mixed Model. Ph.D. Dissertation, NC State University, Raleigh, U.S.A

Zhu, J. 1993. Methods of predicting genotype value and heterosis for offspring of hybrids. (Chinese). Journal of Biomathematics, 8(1): 32-44.
}
