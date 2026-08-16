% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cumulative_episodes}
\alias{cumulative_episodes}
\title{cumulative_episodes}
\usage{
cumulative_episodes(histories, states = .STATES)
}
\arguments{
\item{histories}{A vector; its length is taken and its elements indexed.}
\item{states}{Defaults to \code{.STATES}.}
}
\description{
A step of the bhltmsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{cumulative}, \code{states}, \code{periods}, \code{note}.
}
\keyword{internal}
\examples{
cumulative_episodes(histories = list(a = 1, b = 2))
}
