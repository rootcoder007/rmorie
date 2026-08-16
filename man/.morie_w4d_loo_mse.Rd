% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_loo_mse}
\alias{.morie_w4d_loo_mse}
\title{.morie_w4d_loo_mse}
\usage{
.morie_w4d_loo_mse(D, t)
}
\arguments{
\item{D}{A matrix; passed to \code{t}.}
\item{t}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_unsupervised_pretraining}. See the file header for the source the module follows.
}
\value{
A list with \code{theta}, \code{loo}, \code{train}.
}
\keyword{internal}
\examples{
.morie_w4d_loo_mse(D = 5L, t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
