% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_kdfe_h}
\alias{.fz_kdfe_h}
\title{Bandwidth for a DISTRIBUTION-function-type estimator:}
\usage{
.fz_kdfe_h(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
4^(1/3) sigma n^(-1/3). A cube root, not the fifth root of the density rule. Equations (2.3)-(2.4) put the bandwidth in the variance at O(h/n) with a NEGATIVE sign -- smoothing REDUCES variance here -- rather than at O(1/(nh)), so minimising the MISE gives h_opt = (2 r_1 / (n mu_2^2 R(f\\')))^(1/3), which for a Gaussian kernel against a normal reference is 4^(1/3) sigma n^(-1/3). Sec. 5.3.2 states the same conclusion in words, citing Azzalini (1981). Identical to .morie_kdfe_h in aaa_helpers_fauzi.R; kept local so this file stands alone.
}
\value{
A numeric value.
}
\keyword{internal}
