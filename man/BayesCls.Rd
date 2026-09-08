% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BayesCls}
\alias{BayesCls}
\title{Eq (10.70): d_i(x) = p(x|C_i) P(C_i), assign to the largest -- the}
\usage{
BayesCls(likelihoods, priors = NULL) 
}
\arguments{
\item{likelihoods}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{priors}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
MAXIMUM A POSTERIORI rule.  Comparing likelihoods alone is maximum likelihood, a different classifier, and they differ whenever the classes are unequally common: for a rare disease the prior is precisely what stops the classifier calling everything positive.
}
\value{
A list with \code{d}, \code{posterior}, \code{assigned}, \code{maximum_likelihood_choice}, \code{prior_changed_the_decision}, \code{priors}, \code{uniform_priors}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
BayesCls(V)
}
