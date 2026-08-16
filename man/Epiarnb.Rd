% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Epiarnb}
\alias{Epiarnb}
\title{Epidemic log-autoregression with neighbours (Lawson eq. 18.4)}
\usage{
Epiarnb(beta0, beta1, i_lag, nb_lag, b1)
}
\description{
Epidemic log-autoregression with neighbours (Lawson eq. 18.4)
}
\value{
A list with \code{logf}, \code{f}, \code{total_lag}, \code{n}.
}
\keyword{internal}
\examples{
Epiarnb(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8), i_lag = c(1, 2, 3, 4, 5, 6, 7, 8), nb_lag = c(1, 2, 3, 4, 5, 6, 7, 8), b1 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
