% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.miord2_norm_draw}
\alias{.miord2_norm_draw}
\title{.miord2_norm_draw}
\usage{
.miord2_norm_draw(e, X_obs, y_obs, X_mis, kappa)
}
\arguments{
\item{e}{Passed to \code{.ghc_gamma1}.}
\item{X_obs}{A matrix; passed to \code{ncol}.}
\item{y_obs}{A matrix; passed to \code{\%*\%}.}
\item{X_mis}{A matrix; indexed by row and column.}
\item{kappa}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the miord2_native implementation. Called by \code{morie_miord2}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
