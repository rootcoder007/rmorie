% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kendall_tau}
\alias{kendall_tau}
\title{kendall_tau}
\usage{
kendall_tau(scores, human) 
}
\arguments{
\item{scores}{Passed to \code{.comet_vec}.}
\item{human}{Passed to \code{.comet_vec}.}
}
\description{
A step of the comet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tau}, \code{concordant}, \code{discordant}, \code{n_segments}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
kendall_tau(V, V)
}
