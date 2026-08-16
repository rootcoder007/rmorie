% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{split_specific_tmle}
\alias{split_specific_tmle}
\title{Eq. (9.9)-(9.13): one split\'s TMLE at fixed levels}
\usage{
split_specific_tmle(y, A, W, levels, aL, aH, fit_rows, est_rows, ridge = 1e-08, trim = 0.01, target = TRUE)
}
\arguments{
\item{y}{Outcome vector.}
\item{A}{Exposure vector.}
\item{W}{Covariate matrix.}
\item{levels}{Candidate levels.}
\item{aL}{Lower level.}
\item{aH}{Upper level.}
\item{fit_rows}{Rows to fit Q and g on.}
\item{est_rows}{Rows to fit the fluctuation and average over.}
\item{ridge}{Ridge regulariser.}
\item{trim}{Propensity trimming.}
\item{target}{Whether to target.}
}
\description{
Eq. (9.9)-(9.13): one split\'s TMLE at fixed levels
}
\value{
\code{psi}, \code{D}, info with \code{eps} and   \code{max_weight}.
}
\keyword{internal}
