% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{shrunk_targeting_unsafe}
\alias{shrunk_targeting_unsafe}
\title{Penalise epsilon itself -- the thing not to do}
\usage{
shrunk_targeting_unsafe(Q, H, Y, ridge = 1)
}
\arguments{
\item{Q}{Initial outcome regression.}
\item{H}{Clever covariate.}
\item{Y}{Outcome.}
\item{ridge}{Ridge added to both the score and the Hessian.}
}
\description{
Kept so the cost is measurable: shrinking the fluctuation pulls the estimator back toward the untargeted plug-in and leaves P_n D^* non-zero.
}
\value{
A list with \code{epsilon}, \code{Q_star}, \code{score},   \code{caveat}.
}
\keyword{internal}
\examples{
shrunk_targeting_unsafe(Q = 0.5, H = 0.5, Y = 5L)
}
