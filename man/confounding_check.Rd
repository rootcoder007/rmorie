% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{confounding_check}
\alias{confounding_check}
\title{confounding_check}
\usage{
confounding_check(covariate_history, treatment_history, outcome = NULL) 
}
\arguments{
\item{covariate_history}{A matrix; passed to \code{as.matrix}.}
\item{treatment_history}{A matrix; passed to \code{as.matrix}.}
\item{outcome}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bhltmsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
confounding_check(M, M)
}
