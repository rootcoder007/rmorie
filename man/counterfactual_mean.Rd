% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{counterfactual_mean}
\alias{counterfactual_mean}
\title{counterfactual_mean}
\usage{
counterfactual_mean(Y, A, L, a_star, strata_probs = NULL) 
}
\arguments{
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{L}{Coerced to character by the body, with \code{as.character}.}
\item{a_star}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{strata_probs}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the tlgcmp_native implementation. Called by \code{morie_tlgcmp}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
\examples{
counterfactual_mean(Y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8),
  L = c(1, 2, 3, 4, 5, 6, 7, 8), a_star = c(1, 2, 3, 4, 5, 6, 7, 8))
}
