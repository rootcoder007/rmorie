% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_baseline}
\alias{.morie_cox_baseline}
\title{.morie_cox_baseline}
\usage{
.morie_cox_baseline(t, e, X, beta, offset = NULL)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{e}{See Usage.}
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
\item{offset}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the cox_native implementation. Called by \code{morie_cox_breslow_step}, \code{morie_cox_frailty}, \code{morie_cox_martingale_residuals} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{times}, \code{hazard}, \code{cumhazard}.
}
\keyword{internal}
