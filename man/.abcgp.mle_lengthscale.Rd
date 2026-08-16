% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.mle_lengthscale}
\alias{.abcgp.mle_lengthscale}
\title{.abcgp.mle_lengthscale}
\usage{
.abcgp.mle_lengthscale(X, y, nugget, kernel)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Passed to \code{.abcgp.profile_nll}.}
\item{nugget}{Passed to \code{.abcgp.as_nugget}.}
\item{kernel}{Passed to \code{.abcgp.profile_nll}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{ls}, as built in the body.
}
\keyword{internal}
