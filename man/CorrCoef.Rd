% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CorrCoef}
\alias{CorrCoef}
\title{Bsastat}
\usage{
CorrCoef(x, y)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_ccf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{r}, \code{r_squared}, \code{n}, \code{means}, \code{cosine_without_removing_means}, \code{means_are_removed}, \code{invariant_to_positive_affine_change}, \code{says_nothing_about_agreement}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CorrCoef(V, V)
}
