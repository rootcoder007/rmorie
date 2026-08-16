% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_naive_marginal_mean}
\alias{.schab_naive_marginal_mean}
\title{G^-1(x\\'beta) -- what the marginal mean is NOT, in a GLMM}
\usage{
.schab_naive_marginal_mean(X, beta, link_kind = "log")
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{beta}{See Usage.}
\item{link_kind}{Passed to \code{.schab_link}. Defaults to \code{"log"}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spglmm}. See the file header for the source the module follows.
}
\value{
The value of \code{.schab_link}.
}
\keyword{internal}
