% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_conditional_mean}
\alias{.schab_conditional_mean}
\title{.schab_conditional_mean}
\usage{
.schab_conditional_mean(X, beta, S, link_kind = "log")
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{S}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{link_kind}{Passed to \code{.schab_link}. Defaults to \code{"log"}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spglmm}. See the file header for the source the module follows.
}
\value{
The value of \code{.schab_link}.
}
\keyword{internal}
\examples{
.schab_conditional_mean(X = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, S = c(1, 2, 3, 4, 5, 6, 7, 8))
}
