% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Menorm}
\alias{Menorm}
\title{Measurement-error normal outcome model (Lawson eq. 16.1)}
\usage{
Menorm(beta0, beta1, x_true, tau)
}
\description{
Measurement-error normal outcome model (Lawson eq. 16.1)
}
\value{
A list with \code{mu}, \code{var}, \code{sd}, \code{n}.
}
\keyword{internal}
\examples{
Menorm(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8), x_true = c(1, 2, 3, 4, 5, 6, 7, 8), tau = 0.5)
}
