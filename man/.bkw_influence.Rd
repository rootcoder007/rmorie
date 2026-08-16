% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bkw_influence}
\alias{.bkw_influence}
\title{Hat values, residuals and (X\\\'X)^-1 for the OLS fit of y on X.}
\usage{
.bkw_influence(y, X, intercept = TRUE)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{intercept}{Defaults to \code{TRUE}.}
}
\description{
Returns a list with n, p, e, h, sse and inv (inv[[k]] is COLUMN k of the inverse), shared by the DFFITS, DFBETAS and COVRATIO diagnostics.
}
\value{
A list with \code{n}, \code{p}, \code{e}, \code{h}, \code{sse}, \code{inv}, \code{D}.
}
\keyword{internal}
