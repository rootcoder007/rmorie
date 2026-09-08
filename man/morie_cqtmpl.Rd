% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cqtmpl}
\alias{morie_cqtmpl}
\title{EM for the composite likelihood (equation 6) at one QTL position}
\usage{
morie_cqtmpl(y, left, right, r_left, r_right, cofactors = list(), max_iter = 200L, 
    tol = 1e-10) 
}
\arguments{
\item{y}{Numeric vector of phenotypes.}
\item{left}{Numeric vector of left-marker genotype indicators.}
\item{right}{Numeric vector of right-marker genotype indicators.}
\item{r_left}{Recombination fraction to the left marker.}
\item{r_right}{Recombination fraction to the right marker.}
\item{cofactors}{Optional list of cofactor vectors, each length   \code{length(y)}.}
\item{max_iter}{Maximum EM iterations.}

\item{tol}{Argument \code{tol}; see Usage.}
}
\description{
Fits the CIM model of Zeng (1994) eq. (5)-(6) at a single interval by iterating between the mixture posterior and a weighted regression. With an empty cofactor set it is exactly interval mapping, to machine precision.
}
\value{
A list with the LOD score, fitted coefficients, the   posterior probabilities, the null log-likelihood, and the   iteration history.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_cqtmpl(y = rnorm(10), left = rnorm(10), right = rnorm(10), r_left = rnorm(10),
  r_right = rnorm(10))
TRUE
}
